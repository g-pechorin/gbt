package peterlavalle.gbt

import java.io.File

import junit.framework.TestCase
import org.junit.Assert.assertEquals
import peterlavalle.Mercurial

import scala.io.Source

trait TIntegrationTest extends TestCase with peterlavalle.TPackage {

	lazy val testPath: String = s"integration-tests/${getClass.getSimpleName}"

	lazy val rootProject: File = {

		val parentProject: File = {

			def recu(someProject: File): File =
				if ((someProject / "gradlew").exists())
					someProject
				else recu(someProject.getAbsoluteFile.getParentFile)

			recu(new File("src/"))
		}

		Set("build.gradle").foreach {
			thing =>
				require(
					(parentProject / testPath / thing).exists(),
					s"My assumptions ain't good enuff - couldn't find $testPath / $thing"
				)
		}
		parentProject
	}

	def label: String = getClass.getSimpleName + "/" + getName

	def gradleGoals(from: File, goals: String*): Unit = {
		if (rootProject != from)
			gradleGoals(rootProject, "assemble")

		goals.foreach {
			goal =>

				println(
					"TODO; push the version into this integration test ... somehow"
				)


				def isRelease(from: File): Boolean = {
					val file: File = from / "build/gbt.version"
					if (file.exists())
						!(Source.fromFile(file).mkString.trim == Mercurial.of(from).branch)
					else
						isRelease(from.ParentFile)
				}

				assertEquals(
					0,
					from
						.Shell((rootProject / "gradlew").AbsolutePath)
						.newArg(s"-Drelease=${isRelease(rootProject.ParentFile)}")
						.newArg(goal)
						.newArg("--full-stacktrace")
						.invoke(label + "@" + goal + ">", label + "@" + goal + "!")
				)
		}
	}
}

