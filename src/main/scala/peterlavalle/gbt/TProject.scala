package peterlavalle.gbt

import java.io.File

import org.gradle.api.Project

/**
	* generic interface
	*/
trait TProject {
	def name: String

	def version: String

	def root: TProject

	def buildDir: File

	def home: File
}

object TProject {

	case class Gradle(project: Project) extends TProject {
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
