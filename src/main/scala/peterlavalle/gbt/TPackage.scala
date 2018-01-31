package peterlavalle.gbt

import java.io.File

import org.codehaus.groovy.runtime.InvokerHelper
import org.gradle.api.file.SourceDirectorySet
import org.gradle.api.plugins.{Convention, ExtensionContainer}
import org.gradle.api.tasks.SourceSet
import org.gradle.api.{Action, Project, Task}

import scala.collection.immutable.Stream.Empty
import scala.reflect.ClassTag

/**
	* implicit code for gradle plugins
	*/
trait TPackage extends peterlavalle.TPackage {


	lazy val osName: String =
		System.getProperty("os.name").split("[^\\w]")(0).toLowerCase

	lazy val osArch: String =
		System.getProperty("os.arch").toLowerCase

	implicit class WrappedProject(val project: Project) extends TProperTask.TProperProject {

		/**
			* Scans "up" until a gradle project file is found
			*/
		def buildFile: File = {
			def recu(potential: Project): File = {
				val potentialFile: File = potential.getBuildFile
				if (potentialFile.exists())
					potentialFile
				else {
					recu(potential.getParent)
				}
			}

			recu(project)
		}

		def createTask[T <: Task](name: String)(action: T => Unit)(implicit classTag: ClassTag[T]): T =
			project.getTasks.create[T](
				name,
				classTag.runtimeClass.asInstanceOf[Class[T]],
				new Action[T] {
					override def execute(t: T): Unit = action(t)
				}
			)

		def ext[E](implicit addExtension: Boolean = false, classTag: ClassTag[E]): E = {

			assume(
				!addExtension,
				"OLD - it should be auto-added already!"
			)

			val extClass: Class[E] = classTag.runtimeClass.asInstanceOf[Class[E]]

			require("Config" == extClass.getSimpleName)

			// it's tempting to try and use the root project ... hmm ...
			val extensions: ExtensionContainer = project.getExtensions

			extensions.findByType[E](extClass) match {
				case null =>
					???

				case mine: E =>
					mine
			}
		}

		def theTask[T <: Task](name: String)(implicit classTag: ClassTag[T]): T =
			findTask(name) match {
				case Some(task: T) => task
			}

		def findTask[T <: Task](name: String)(implicit classTag: ClassTag[T]): Option[T] =
			project.getTasks.find {
				(task: Task) =>
					(name == null || name == task.getName) && classTag.runtimeClass.isInstance(task)
			} match {
				case Some(task: T) => Some(task)
				case None => None
			}

		def allTasksOf[T <: Task](implicit kind: ClassTag[T]): Stream[T] =
			project.getAllTasks(false).toStream.flatMap {
				case (owner, _) if owner != project =>
					Empty

				case (_, tasks) =>
					tasks.toStream.filter {
						case task: T =>
							require(task.isInstanceOf[T])
							require(kind.runtimeClass.isInstance(task))
							true
						case _ =>
							false
					}.map(_.asInstanceOf[T])
			}
	}

	/**
		* assumes that your package name indicates something about the sourceSet
		*
		* `peterlavalle.foo` implies `sourceSets.foo...`
		*/
	protected lazy val extensionName: String = getClass.getPackage.getName.split("\\.").last

	implicit class GWrappedObject(value: AnyRef) {
		def gProperty[T](path: String)(implicit classTag: ClassTag[T]): T =
			path.split("\\.").foldLeft(value) {
				case (self, name) =>
					InvokerHelper.getProperty(self, name)
			} match {
				case found: T => found
			}
	}

	implicit class WrappedSourceSet(sourceSet: SourceSet) {
		def theSourceSet(name: String): SourceDirectorySet =
			InvokerHelper.getProperty(sourceSet, name) match {
				case win: SourceDirectorySet => win
				case wat =>
					s"wat = ${wat.getClass}  (${wat.toString})" halt
			}

		def displayName: String =
			InvokerHelper.invokeMethod(sourceSet, "getDisplayName", null.asInstanceOf[Any]).asInstanceOf[String]

		def sourceSetConvention: Convention =
			InvokerHelper.getProperty(sourceSet, "convention").asInstanceOf[Convention]
	}

}
