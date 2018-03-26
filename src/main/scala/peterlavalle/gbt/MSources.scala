package peterlavalle.gbt

import java.io.File
import java.util
import java.util.concurrent.Callable

import org.gradle.api.file.SourceDirectorySet
import org.gradle.api.internal.file.SourceDirectorySetFactory
import org.gradle.api.plugins.JavaPluginConvention
import org.gradle.api.tasks.{SourceSet, TaskContainer}
import org.gradle.api.{DefaultTask, GradleException, Project, Task}
import peterlavalle.Later
import peterlavalle.gbt.TProperTask.{Phase, Source}

import scala.reflect.ClassTag

/**
	* contains logic for working with project source code
	*/
object MSources {

	trait MPlugin {
		this: APlugin =>

		val sourceDirectorySetFactory: SourceDirectorySetFactory

		def sourceSet(name: String)(extensions: String*): Unit =
			configure {
				addSourceSet(name, sourceDirectorySetFactory, project) {
					(_: SourceSet, theDirectorySet: SourceDirectorySet) =>
						extensions.foreach {
							extension: String =>
								extension.head match {
									case '.' =>
										theDirectorySet.include(
											s"**/*$extension"
										)

									case '!' =>
										theDirectorySet.include(extension.tail)

									case _ =>
										theDirectorySet.include(extension)
								}
						}
				}
			}

		private final def addSourceSet
		(
			kind: String,
			sourceDirectorySetFactory: SourceDirectorySetFactory,
			project: Project
		)(detail: (SourceSet, SourceDirectorySet) => Unit): Unit =
			project.getConvention.getPlugin(classOf[JavaPluginConvention]).getSourceSets.all(
				(sourceSet: SourceSet) => {

					val displayName: String = sourceSet.displayName

					val phase: TProperTask.Phase =
						displayName match {
							case "main" => TProperTask.Phase.Main
							case "test" => TProperTask.Phase.Test
						}

					// setup a source set
					val theSourceSet: APlugin.TSourceSet =
						spawnSourceSet(kind, displayName)

					sourceSet.sourceSetConvention.getPlugins.put(kind, theSourceSet)

					// also ; setup a source set
					val theDirectorySet: SourceDirectorySet = theSourceSet.src
					theDirectorySet.srcDir(
						new Callable[File]() {
							@throws[Exception]
							override def call: File = project.file(s"src/$displayName/$kind/")
						}
					)
					sourceSet.getAllSource.source(theDirectorySet)

					// do the detail!
					detail(sourceSet, theDirectorySet)

					// create the generate Task(s)
					val genTas: MSources =
						project.createTask[MSources](s"gen_$kind$phase") {
							gen: MSources =>
								gen._kind = kind
								gen._phase = phase
						}
				}
			)

		private final def spawnSourceSet(kind: String, displayName: String): APlugin.TSourceSet =
			APlugin.spawnCache(kind)(displayName, sourceDirectorySetFactory.create(displayName + s" $kind Source"))
	}

	trait MTask {

		this: TProperTask =>

		def produce[V](source: String)(lambda: File => V)(implicit classTag: ClassTag[V]): Later[V]

		final def produce[V](source: String, phase: Phase)(lambda: File => V)(implicit classTag: ClassTag[V]): Later[V] = {

			genTask(phase, source).dependsOn(this)

			// create an output folder
			val genOut: File = {
				val genOut: File =
					(getProject.getBuildDir / s"generated-src/$getName-$source")
						.EnsureExists

				getProject
					.gProperty[SourceDirectorySet](s"sourceSets.${phase.toString.toLowerCase}.$source")
					.srcDir(genOut)

				genOut.EnsureExists
			}

			perform {
				lambda(genOut)
			}
		}

		/**
			*
			* looks up the MSourceSet task for a given phase/kind
			*
			* @param phase can be either phase
			* @param kind  can be any kind that has been added
			* @return the proper MSourceSet task
			*/
		private final def genTask(phase: Phase, kind: String): MSources = {
			val tasks: TaskContainer = getProject.getTasks
			val taskSet: util.Set[Task] = tasks.asInstanceOf[util.Set[Task]]

			val array: Array[AnyRef] = taskSet.toArray


			val taskSeq: List[Task] =
				array.indices
					.map((i: Int) => array(i))
					.map { case (t: Task) => t }
					.toList

			val matches: List[MSources] =
				taskSeq
					.filter {
						case m: MSources =>
							m.phase == phase && m.kind == kind
						case _ => false
					}
					.map {
						case m: MSources => m
					}

			matches match {
				case List(task: MSources) => task
				case Nil =>
					throw new GradleException(
						taskSeq
							.map {
								t: Task =>
									t.getName + "(" + t.getClass.getSimpleName + ")"
							}
							.foldLeft(s"I could not find a gen task for `$kind` phase $phase")((_: String) + "\n\t\t\t" + (_: String))
					)
			}
		}

		def consume[O](source: String)(lambda: Iterable[Source] => O)(implicit classTag: ClassTag[O]): Later[O]

		/**
			* consumes source to produce some output
			*/
		final def consume[O](phase: Phase, source: String)(operation: Iterable[Source] => O)(implicit classTag: ClassTag[O]): Later[O] = {

			// depend upon source generation
			dependsOn(genTask(phase, source))

			// line up the actual action
			perform {

				// get the compiled stuff, pass it along
				phase match {
					case Phase.Main | Phase.Test =>
						val sourceDirectorySets: Stream[SourceDirectorySet] =
							Stream(sourceSet(getProject, phase, source))

						val fullStream: Stream[Stream[Source]] =
							sourceDirectorySets.map {
								sourceDirectorySet: SourceDirectorySet =>
									// big, stupid, list of all possible files
									val knownFiles: Set[String] =
										sourceDirectorySet.getAsFileTree.map((_: File).AbsolutePath).toSet
											.filterNot((_: String).matches(".*/[\\._].*"))

									// now ... filter all contained files by the name ... an imperfect solution
									sourceDirectorySet.getSrcDirs.toStream.flatMap {
										root: File =>
											(root **).filter {
												path: String =>
													knownFiles((root / path).AbsolutePath)
											}.map(root -> (_: String))
									}
							}

						operation(
							fullStream.flatten.distinctBy {
								case (_, path: String) =>
									path
							}.toList
						)
				}
			}
		}

		private final def sourceSet(project: Project, phase: Phase, source: String): SourceDirectorySet =
			project.gProperty[SourceDirectorySet](s"sourceSets.${phase.toString.toLowerCase}.$source")
	}

}

class MSources extends DefaultTask {

	// TODO; can this be handled through parameter injection somehow?
	private var _kind: String = _
	private var _phase: TProperTask.Phase = _

	def kind: String = _kind

	def phase: TProperTask.Phase = _phase
}
