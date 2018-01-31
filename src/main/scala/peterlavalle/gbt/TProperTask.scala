package peterlavalle.gbt

import java.io.File

import org.gradle.api._
import org.gradle.api.artifacts.{Configuration, ConfigurationContainer, Dependency, ProjectDependency}
import org.gradle.api.file.SourceDirectorySet
import org.gradle.api.tasks.TaskAction
import peterlavalle.gbt.TProperTask.Phase
import peterlavalle.{Later, RunnableFuture}

import scala.reflect.ClassTag
import scala.util.matching.Regex

/**
	* base of all tasks in my mind.
	*
	* - can consume other tasks
	* - should be setup with a "install" method and never by hand!
	*/
sealed abstract class TProperTask(group: String, description: String) extends DefaultTask with TPackage {

	setGroup(group)
	setDescription(description)

	private val actionRunnables: RunnableFuture = new RunnableFuture()
	private val connectRunnables: RunnableFuture = new RunnableFuture()

	def depsImediate[T](phase: Phase)(lambda: List[Dependency] => T)(implicit classTag: ClassTag[T]): T = {

		val configurations: ConfigurationContainer =
			getProject.getConfigurations

		val Some(configuration: Configuration) =
			configurations.find((_: Configuration).getName == {
				phase match {
					// TODO ; should I worry about runtime/compileOPnly type deps?
					case Phase.Main =>
						"compile"

					case Phase.Test =>
						"testCompile"
				}
			})

		lambda(
			configuration.getDependencies.toList
		)
	}

	/**
		* locate an extension object
		*/
	def ext[E](implicit classTag: ClassTag[E]): E =
		getProject.ext[E]

	def sibling[T <: TProperTask](implicit classTag: ClassTag[T]): Later[T] =
		TProperTask.dependConsumeTasks(this) {
			this match {
				case phased: TProperTask.TTaskPhased =>
					getProject.getTasks.toList.filterTo[T].filter {
						case task: TProperTask.TTaskPhased =>
							task.phase == phased.phase
					}

				case single: TProperTask.TTaskSingle =>
					getProject.getTasks.toList.filterTo[T].filter {
						case task: TProperTask.TTaskSingle =>
							true
					}
			}
		}.wrap {
			case List(one) => one
		}

	final def findPhasedTask[T <: TProperTask.TTaskPhased](phase: Phase)(implicit classTag: ClassTag[T]): T = {

		val allTasks: Stream[Task] =
			getProject.getAllTasks(false).toStream.flatMap {
				case (l, r) =>
					r
			}

		allTasks.filterTo[T].sortBy((_: T).phase.toString) match {
			case Nil =>
				throw new GradleException(
					allTasks
						.map((_: Task).getClass.getName)
						.foldLeft(
							s"No tasks of type `${classTag.runtimeClass.getName}` from ${getClass.getName}"
						)((_: String) + "\n\t>" + (_: String) + "<")
				)
			case list =>
				list.filter((_: T).phase == phase) match {
					case List(one) =>
						one
				}
		}
	}

	final def findPhasedGradleTask(prefix: String, phase: Phase, suffix: String): Task = {

		val findName: String =
			phase match {
				case Phase.Main =>
					prefix + suffix
				case Phase.Test =>
					prefix + "Test" + suffix
			}

		val foundTask: Task =
			getProject.getTasks.findByName(
				findName
			)

		requyre[GradleException](null != foundTask, s"Couldn't find task with name `$findName`")

		foundTask
	}

	/**
		* create a lambda that performs something during execution and (possibly) returns a value
		*/
	final def perform[T](lambda: => T): Later[T] = {

		val setOnce: Later.SetOnce[T] =
			new Later.SetOnce[T]()
				.withError {
					s"have not performed $getPath (do you need to depend on it?)"
				}

		actionRunnables ! {
			setOnce := lambda
		}

		setOnce.later
	}

	/**
		* create a lambda that does something as part of task setup
		*/
	final def connect(lambda: => Unit): Unit =
		connectRunnables.add(
			new Runnable {
				override def run(): Unit = {
					lambda
				}
			}
		)

	@TaskAction
	final def taskAction(): Unit = actionRunnables.run()
}

/**
	*
	*/
object TProperTask extends TPackage {

	type Source = (File, String)

	// "cheat" to help me hide the runnables
	sealed implicit class SneakTProperTask(properTask: TProperTask) {

		// "cheat" to help me hide the runnables
		def ActionRunnables: RunnableFuture = properTask.actionRunnables
	}

	final def configurationAction[T <: TProperTask]: Action[T] =
		new Action[T] {
			override def execute(task: T): Unit = {

				// check that the task isn't trying to do both things
				if (task.isInstanceOf[TTaskPhased])
					require(!task.isInstanceOf[TTaskSingle])
				else
					require(task.isInstanceOf[TTaskSingle])

				task.connectRunnables.run()
			}
		}

	/**
		* Peels apart the current project into a stream of dependencies
		**/
	def disectProject(project: Project, phase: Phase, source: String): Stream[SourceDirectorySet] = {

		/**
			* convert our "project" into a stream of "projects" so that we visit our deps
			*/
		def directDependencyProjects: Stream[Project] =
			TProperTask.grabProjects(
				project,
				phase match {
					case Phase.Test =>
						true
					case Phase.Main =>
						false
				}
			)

		/**
			* list the source sets from this project
			*/
		def mine: Stream[SourceDirectorySet] =
			Set(phase, Phase.Main).toStream.map(p => sourceSet(project, p, source))

		if (false) // dump used for debugging
		// (I kept this because I'm a horrible person - delete this block if you see it and never look back)
			println(
				s"""
					 |project:${project.getName}
					 |src:
					 |${mine.flatMap(_.getSrcDirs.map(_.AbsolutePath)).foldLeft("")(_ + "\t" + _ + "\n")}
					 |lib:
					 |${directDependencyProjects.map(_.getName).foldLeft("")(_ + "\t" + _ + "\n")}
									""".stripMargin
			)

		// BOOM!
		mine ++ (directDependencyProjects.flatMap(p => disectProject(p, Phase.Main, source)))
	}

	def sourceSet(project: Project, phase: Phase, source: String): SourceDirectorySet =
		project.gProperty[SourceDirectorySet](s"sourceSets.${phase.toString.toLowerCase}.$source")

	private final def grabProjects(project: Project, test: Boolean = false): Stream[Project] = {
		grabProjects(project,
			if (test)
				Seq("compile", "testCompile")
			else
				Seq("compile")
		)
	}

	private final def grabProjects(project: Project, configurations: Seq[String]): Stream[Project] = {
		configurations
			.flatMap {
				name: String =>
					project.getConfigurations.getByName(name).getAllDependencies
			}.map {
			case projectDependency: ProjectDependency =>
				projectDependency.getDependencyProject
		}.toStream
	}

	/**
		* configures @self to depend on all @tasks and returns a Later that will contain them all
		*/
	private def dependConsumeTasks[T <: TProperTask](self: TProperTask)(tasks: Iterable[T])(implicit classTag: ClassTag[T]): Later[List[T]] = {

		// we're using a later to enforce good behaviour
		val setOnce: Later.SetOnce[List[T]] = new Later.SetOnce[List[T]]

		// we'll (obviously) depend on each of these tasks
		tasks.foreach {
			(task: T) =>
				self.dependsOn(task)
		}

		// the tasks (should) have already run - so just copy them out here
		self.ActionRunnables ! {
			setOnce := tasks.toList
		}

		// we're ready!
		setOnce.later
	}

	@deprecated("stop consuming source from other projects", "2017-10-19")
	sealed trait Consumption

	sealed trait Phase

	// allows installing our tasks
	trait TProperProject {
		val project: Project

		def findPhasedTasks[T <: TProperTask.TTaskPhased](phase: TProperTask.Phase)(implicit classTag: ClassTag[T]): List[T] =
			project
				.findTasks[T]
				.filter((_: T).phase == phase)


		def findTasks[T <: Task](implicit classTag: ClassTag[T]): List[T] =
			project
				.getTasks
				.filterTo[T]

		/**
			* the correct way of setting up a proper task
			*/
		def install[T <: TProperTask](implicit classTag: ClassTag[T]): Unit = {

			def suffixName(implicit taskClass: Class[_ <: TProperTask]): String =
				taskClass.getName.split("\\.").last.split("\\$").last match {
					case decorated: String if decorated.endsWith("Task_Decorated") =>
						decorated.dropRight("Task_Decorated".length)
					case trueClass: String if trueClass.endsWith("Task") =>
						trueClass.dropRight("Task".length)
					case plain: String =>
						plain
				}

			def prefixPackage(implicit taskClass: Class[_ <: TProperTask]): String =
				taskClass.getName.split("\\.").reverse.tail.head

			implicit val taskClass: Class[T] = classTag.runtimeClass.asInstanceOf[Class[T]]

			require(suffixName.head.isUpper)
			prefixPackage.foreach((c: Char) => require(!c.isUpper, s"prefixPackage = `$prefixPackage`"))

			val names: List[(String, String)] =
				if (classOf[TTaskSingle].isAssignableFrom(taskClass))
					List((prefixPackage + suffixName, null))
				else
					TProperTask.Phase.each {
						(phase: Phase) =>
							val taskName: String = prefixPackage + phase.toString + suffixName

							if (Phase.Main == phase)
								(taskName, null)
							else
								(taskName, prefixPackage + Phase.Main.toString + suffixName)
					}

			names.foreach {
				case (taskName: String, usesName) =>
					val task: T =
						project.getTasks.create(taskName, taskClass, TProperTask.configurationAction[T])

					task.actionRunnables.lock

					require(
						taskName == task.getName
					)

					if (null != usesName)
						task.dependsOn(
							project.getTasks.findByName(usesName)
						)
			}
		}

	}

	abstract class TTaskSingle(group: String, description: String) extends TProperTask(group: String, description: String) {
		/**
			* consume all tasks in us or our children
			*/
		def subTree[T <: TProperTask](implicit classTag: ClassTag[T]): Later[List[T]] =
			dependConsumeTasks(this) {
				Stream(getProject).explode(_.getSubprojects).flatMap {
					project: Project =>
						project.getTasks.toList.filterTo[T]
				}
			}

		def consumePhased[T <: TTaskPhased](implicit classTag: ClassTag[T]): Later[List[T]] = {
			dependConsumeTasks[T](this) {
				getProject.getTasks.filterTo[T]
			}
		}
	}

	abstract class TTaskPhased(group: String, description: String) extends TProperTask(group: String, description: String) {


		lazy val phase: Phase =
			getName match {
				case Phase.rPhase(name) =>
					name match {
						case "Main" => Phase.Main
						case "Test" => Phase.Test
					}
			}

		def depsImediate[T](lambda: List[Dependency] => T)(implicit classTag: ClassTag[T]): T = {
			depsImediate(phase)(lambda)
		}

		/**
			* consumes main from us and any dependencies
			* if we're a `test` task this consumes our test also
			* force-skips itself so can use to get siblings
			*/
		final def consume[T <: TProperTask.TTaskPhased](implicit classTag: ClassTag[T]): Later[List[T]] = {
			// find the list of tasks
			dependConsumeTasks(this) {

				// we want our tasks
				val ourTasks: List[T] =
					getProject.getTasks.toList.filterTo[T].filter {
						task: T =>
							task.phase == Phase.Main || task.phase == this.phase
					}

				// ... and we want OTHER tasks!
				val dependencyTasks: Stream[T] = {

					def disect(project: Project, configurations: List[String]): Stream[T] = {

						val head: Stream[T] =
							project.getTasks.toList.filterTo[T].toStream

						val tail: Stream[T] =
							configurations.toStream.flatMap {
								name: String =>
									project.getConfigurations.getByName(name).getAllDependencies.flatMap {
										case projectDependency: ProjectDependency =>
											disect(projectDependency.getDependencyProject, List("compile"))
									}
							}

						head ++ tail
					}

					disect(
						getProject,
						phase match {
							case Phase.Main => List("compile")
							case Phase.Test => List("testCompile", "compile")
						}
					)
				}

				// okay, calculate the final output
				(ourTasks.filter(_ eq this).toStream ++ dependencyTasks).distinct
			}
		}

		final def findPhasedGradleTask(prefix: String, suffix: String): Task =
			findPhasedGradleTask(prefix, phase, suffix)

		final def findPhasedTask[T <: TProperTask.TTaskPhased](implicit classTag: ClassTag[T]): T =
			findPhasedTask[T](this.phase)

		final def produce[V](source: String)(lambda: File => V): Later[V] = {
			val setOnce: Later.SetOnce[V] = new Later.SetOnce[V]()
			// create an output folder
			val genOut: File = {
				val genOut: File = (getProject.getBuildDir / s"generated-src/$getName-$source").EnsureExists
				getProject
					.gProperty[SourceDirectorySet](s"sourceSets.${phase.toString.toLowerCase}.$source").srcDir(genOut)
				genOut.EnsureExists
			}

			this.ActionRunnables ! {
				setOnce := lambda(genOut)
			}

			setOnce.later
		}

		final def consume[O](source: String)(operation: Iterable[Source] => O)(implicit classTag: ClassTag[O]): Later[O] = {

			// create a set-once to hold out result
			val setOnce: Later.SetOnce[O] = new Later.SetOnce[O]()

			// line up the actual action
			this.ActionRunnables ! {

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

						setOnce :=
							operation(
								fullStream.flatten.distinctBy {
									case (_, path: String) =>
										path
								}.toList
							)
				}
			}

			// cool - return the later
			setOnce.later
		}

		/**
			* used during construction to denote that we will consume a sourceset
			*/
		@deprecated("stop consuming source from other projects", "2017-10-19")
		final def consume(source: String, consumption: TProperTask.Consumption): Later[List[TProperTask.Source]] = {


			val setOnce: Later.SetOnce[List[Source]] = new Later.SetOnce[List[TProperTask.Source]]()

			this.ActionRunnables ! {
				val sourceDirectorySets: Stream[SourceDirectorySet] =
					consumption match {
						case Consumption.Full =>


							// okay; we can use the function now
							disectProject(getProject, phase, source).distinct

						case Consumption.Self =>
							Stream(sourceSet(getProject, phase, source))
					}

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

				setOnce :=
					fullStream.flatten.distinctBy {
						case (_, path: String) =>
							path
					}.toList
			}

			setOnce.later
		}
	}

	@deprecated("stop consuming source from other projects", "2017-10-19")
	object Consumption {

		/**
			* just stuff in our project
			*/
		case object Self extends Consumption

		/**
			* all in our project, and all in dependencies
			*/
		case object Full extends Consumption

	}

	object Phase {
		val rPhase: Regex =
			"[a-z0-9]+([A-Z][a-z]+)[A-Z].*" r

		def each[V](step: Phase => V): List[V] =
			List(Main, Test).map(step)

		case object Main extends Phase

		case object Test extends Phase

	}
}
