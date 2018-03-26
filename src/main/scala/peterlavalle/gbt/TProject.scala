package peterlavalle.gbt

import java.io.File

import org.gradle.api.plugins.ExtensionContainer
import org.gradle.api.{Action, Project, Task}
import peterlavalle.gbt.TProperTask.{Phase, TTaskSingle}

import scala.collection.immutable.Stream.Empty
import scala.reflect.ClassTag
import scala.util.matching.Regex

/**
	* generic interface
	*/
trait TProject
	extends peterlavalle.TPackage
		with peterlavalle.gbt.TPackage {

	val project: Project

	def name: String

	def version: String

	def root: TProject

	def buildDir: File

	def home: File

	def findPhasedTasks[T <: TProperTask.TTaskPhased](phase: TProperTask.Phase)(implicit classTag: ClassTag[T]): List[T] =
		findTasks[T]
			.filter((_: T).phase == phase)


	def findTasks[T <: Task](implicit classTag: ClassTag[T]): List[T] =
		project
			.getTasks
			.filterTo[T]
			.toList

	/**
		* the correct way of setting up a proper task
		*/
	def install[T <: TProperTask](implicit classTag: ClassTag[T]): Unit = {

		implicit val taskClass: Class[T] =
			classTag.runtimeClass.asInstanceOf[Class[T]]

		val (packageName: String, className: String) = {

			val rClassName: Regex =
				"(\\w+\\.)+(([A-Z]\\w+\\$)*[A-Z]\\w+)" r

			val className: String =
				taskClass.getName match {
					case rClassName(_, className: String, _) =>
						className.replace('$', '.')
				}

			val packageName: String =
				taskClass.getName.split("\\.").reverse.tail.head

			(packageName, className)
		}

		val taskNames: List[(String, String)] =
			if (classOf[TTaskSingle].isAssignableFrom(taskClass))
				List((packageName + className, null))
			else
				TProperTask.Phase.each {
					(phase: Phase) =>
						(packageName + phase + className, if (Phase.Main != phase) packageName + Phase.Main + className else null)
				}

		// create the tasks
		val taskObjects: Map[String, T] =
			taskNames
				.map {
					case (taskName: String, _) =>
						(taskName, project.getTasks.create(taskName, taskClass, TProperTask.configurationAction[T]))
				}
				.toMap

		// connect the tasks
		taskNames
			.foreach {
				case (_: String, null) =>
				case (name: String, uses: String) =>
					taskObjects(name) dependsOn taskObjects(uses)
			}
	}


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

	def createTask[T <: Task](name: String)(action: T => Unit)(implicit classTag: ClassTag[T]): T

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
				}.map((_: Task).asInstanceOf[T])
		}
}

object TProject {

	case class Gradle(project: Project) extends TProject {
		override def createTask[T <: Task](name: String)(action: T => Unit)(implicit classTag: ClassTag[T]): T = {
			project.getTasks.create[T](
				name,
				classTag.runtimeClass.asInstanceOf[Class[T]],
				new Action[T] {
					override def execute(t: T): Unit = action(t)
				}
			)
		}

		override def buildDir: File = project.getBuildDir

		override def version: String = project.getVersion.toString

		override def home: File = project.getProjectDir

		override def root: TProject =
			if (project.getRootProject == project)
				this
			else
				Gradle(project.getRootProject)

		override def name: String = project.getName
	}

}
