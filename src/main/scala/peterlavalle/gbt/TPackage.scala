package peterlavalle.gbt

import org.codehaus.groovy.runtime.InvokerHelper
import org.gradle.api.Project
import org.gradle.api.file.SourceDirectorySet
import org.gradle.api.plugins.Convention
import org.gradle.api.tasks.SourceSet

import scala.reflect.ClassTag

/**
	* implicit code for gradle plugins
	*/
trait TPackage extends peterlavalle.TPackage {


	lazy val osName: String =
		System.getProperty("os.name").split("[^\\w]")(0).toLowerCase

	lazy val osArch: String =
		System.getProperty("os.arch").toLowerCase

	implicit def WrappedProject(project: Project): TProject =
		TProject.Gradle(project)

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
