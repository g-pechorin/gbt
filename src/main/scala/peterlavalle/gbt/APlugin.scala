package peterlavalle.gbt

import java.io.File
import java.util.concurrent.Callable

import org.gradle.api._
import org.gradle.api.file.SourceDirectorySet
import org.gradle.api.internal.file.SourceDirectorySetFactory
import org.gradle.api.plugins.{ExtensionContainer, JavaPluginConvention}
import org.gradle.api.tasks.SourceSet
import peterlavalle.{OverWriter, RunnableFuture}

import scala.beans.BeanProperty
import scala.reflect.ClassTag

object APlugin {

	def upName(name: String): String =
		s"${name.substring(0, 1).toUpperCase()}${name.substring(1)}"

	trait TSourceSet {

		val displayName: String
		val src: SourceDirectorySet

		def Name: String = upName(name)

		@BeanProperty
		def name: String = displayName
	}

}

/**
	* base class for plugins. this (basically) exists to ease the creation of source sets
	*/
trait APlugin extends org.gradle.api.Plugin[Project] with TPackage {

	private val configureRunnables: RunnableFuture = new RunnableFuture()
	private var target: Option[Project] = None

	final override def apply(target: Project): Unit = {

		new OverWriter(target.getRootProject.getBuildDir / "gbt.version")
			.appund(target.getVersion.toString)
			.closeFile

		//
		// check that we're not doing anything trickey
		val extName = getClass.getPackage.getName.split("\\.").last
		requyre[GradleException](
			extName != target.getName,
			s"Can't name the project $extName"
		)
		target.getTasks.foreach {
			task =>
				requyre[GradleException](
					extName != task.getName,
					s"Can't name a task $extName"
				)
		}

		//
		// try to install a config object
		try {
			val packageName: String = getClass.getPackage.getName
			val extClass: Class[_] = Class.forName(s"$packageName.Config")

			// it's tempting to try and use the root project ... hmm ...
			val extensions: ExtensionContainer = target.getExtensions

			extensions.findByType(extClass) match {
				case null =>
					extClass.getConstructors.toList match {
						case List(only) =>
							only.getParameterTypes.toList match {
								case Nil =>
									extensions.create(
										extName,
										extClass
									)

								case List(classOfProject) if classOfProject == classOf[Project] =>
									extensions.create(
										extName,
										extClass,
										target
									)
							}
					}

				case found =>
					;

			}
		} catch {
			case _: ClassNotFoundException => ;
		}

		//
		// run some configuration lambda code
		require(this.target.isEmpty)
		this.target = Some(target)
		configureRunnables.run()
		require(this.target.nonEmpty)
		require(target eq this.target.get)
		this.target = None
	}

	def newSourceSet(kind: String, displayName: String, src: SourceDirectorySet): APlugin.TSourceSet

	final def addSourceSet
	(
		kind: String,
		sourceDirectorySetFactory: SourceDirectorySetFactory,
		project: Project
	)(detail: (SourceSet, SourceDirectorySet) => Unit): Unit =
		project.getConvention.getPlugin(classOf[JavaPluginConvention]).getSourceSets.all(
			new Action[SourceSet]() {
				override def execute(sourceSet: SourceSet): Unit = {

					val displayName: String = sourceSet.displayName

					// setup a source set
					val theSourceSet: APlugin.TSourceSet =
						newSourceSet(kind, displayName, sourceDirectorySetFactory.create(displayName + s" $kind Source"))

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
				}
			}
		)

	final def install[T <: TProperTask](implicit classTag: ClassTag[T]): Unit =
		configure {
			project.install[T]
		}

	final def plugin[P <: Object](implicit classTag: ClassTag[P]): Unit =
		configure {
			project.getPluginManager.apply(classTag.runtimeClass)
		}

	def project: Project =
		target match {
			case Some(project: Project) =>
				project
		}

	final def configure(lambda: => Unit): Unit =
		configureRunnables.add(
			new Runnable {
				override def run(): Unit = {
					lambda
				}
			}
		)

}
