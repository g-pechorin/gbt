package peterlavalle.gbt

import java.io.File
import java.util

import org.gradle.api.Task
import org.gradle.api.artifacts.Configuration
import peterlavalle.{Later, PHile}


object Content
	extends peterlavalle.TPackage
		with TDiagnostics
		with Later.T
		with peterlavalle.gbt.TPackage {

	private val contentPath: String = "META-INF/content"

	sealed class Task extends TProperTask.TTaskSingle(
		"other",
		"places content files onto the classpath, allowing them to be archived"
	) {

		dependencyOf("processResources")

		val contentOutput: Later[File] =
			produce("resources") {
				resources: File =>
					(resources / contentPath)
						.FreshFolder
			}

		private val flavours: util.HashMap[String, Flavour] = new util.HashMap[String, Flavour]()

		override def getDescription: String =
			flavours.keySet().toList.sorted.foldLeft(super.getDescription) {
				(description: String, classifier: String) =>
					s"$description\n\t$classifier\n\t\t${flavours(classifier).configuration.getDescription}"
			}

		implicit class Flavour(val configuration: Configuration) {

			lazy val dependencies: PHile = {
				// resolve the dependencies
				configuration.resolve().toList

					// convert them into PHiles
					.map(PHile.ofZip)

					// cascade them
					.foldRight(PHile.Fresh)((_: PHile) link (_: PHile))

					// select only what's approps
					.subFolder(contentPath + '/' + configuration.getName)
			}

			private lazy val rootOut: File =
				(Task.this.getProject.getBuildDir / s"content/${configuration.getName}")
					.EnsureExists

			val subs: util.HashSet[File] = new util.HashSet[File]()

			def /(path: String): File = {
				val sub: File = rootOut / path
				subs.add(sub.EnsureExists)
				sub
			}
		}

		def apply(classifier: String): Flavour =
			flavours(classifier)

		def ?(classifier: String): Boolean =
			flavours containsKey classifier

		def update(classifier: String, configuration: Configuration): Flavour = {
			require(!flavours.containsKey(classifier))
			require(classifier == configuration.getName)
			flavours(classifier) = configuration
			flavours(classifier)
		}

		perform {

			case class Duplication(classifier: String, contentName: String, taskNames: Set[String])

			// copy and find duplications
			val duplications: Iterable[Duplication] =
				flavours.flatMap {
					case (classifier: String, flavour: Flavour) =>
						flavour.subs.flatMap {
							sub: File =>

								val phile: PHile = PHile.ofFolder(sub)

								// copy out the PHile
								phile.toDir(contentOutput)

								// assemble Duplication objects
								phile.list.map {
									contentName: String =>
										Duplication(
											classifier,
											contentName,
											Set(s"UNKNOWN; put task names in flavour ${sub.AbsolutePath}")
										)
								}
						}
				}
					.groupBy(d => d.classifier + "," + d.contentName)
					.map {
						case (_, duplications: Iterable[Duplication]) =>
							Duplication(
								duplications.head.classifier,
								duplications.head.contentName,
								duplications.flatMap(_.taskNames).toList.sorted.toSet
							)
					}
					.filter(_.taskNames.size > 1)

			//
			if (duplications.nonEmpty)
				STUB("Print a message about duplicate files")
		}
	}

}

trait Content
	extends peterlavalle.TPackage
		with TDiagnostics
		with peterlavalle.gbt.TPackage {

	/**
		* consume+produce; do this from a task to;
		* - declare that there's a sort of content
		* - declare that we're going to expect a PHile aggregating the content, from our dependencies
		* - we may/may not produce new files of content into the passed File object
		* - whatever happens, we'll return a Later instance
		*/
	def content[O](classifier: String, description: String)(lambda: (PHile, File) => O): Later[O] = {

		// declare the content, and, heave it depend upon us
		Internal.contentDeclare(classifier, description)
			.dependsOn(this)

		// setup a consume from dependencies
		val dependencies: Later[PHile] = Internal.contentConsume(classifier)

		// get a folder to use as a source for content
		val output: File = Internal.contentProduce(classifier)

		// do our thing!
		task.perform {
			lambda(dependencies.get, output)
		}
	}

	private def task: TProperTask = this.asInstanceOf[TProperTask]

	/**
		* consume only; do this from a task to;
		* - declare that there's a sort of content
		* - declare that we're going to expect a PHile aggregating the content, from our dependencies and peer modules
		* - whatever happens, we'll return a Later instance
		*/
	def contents[O](classifier: String, description: String)(lambda: PHile => O): Later[O] = {

		// declare that we will depend on the content
		task.dependsOn(
			Internal.contentDeclare(classifier, description)
		)

		// setup a consume from dependencies
		val dependencies: Later[PHile] = Internal.contentConsume(classifier)

		// pickup whatever whatnots we should from the content task
		val outputs: Later[File] =
			Internal.contentReadout(classifier)

		// do our thing!
		task.perform {
			lambda(
				PHile.ofFolder(outputs.get) link dependencies.get
			)
		}
	}

	private object Internal extends Later.T {

		def contentConsume(classifier: String): Later[PHile] = {

			val List(contentTask: Content.Task) =
				task.getProject.findTasks[Content.Task]

			task.perform {
				contentTask(classifier).dependencies
			}
		}

		def contentDeclare(classifier: String, description: String): Task = {

			println(s"contentDeclare($classifier)")

			val contentTask: Content.Task = findContentTask

			if (contentTask ? classifier) {
				require(description == contentTask(classifier).configuration.getDescription)
			} else {
				contentTask(classifier) =
					task.getProject
						.getConfigurations
						.create(classifier)
						.setDescription(description)
						.setTransitive(true)
						.setVisible(false)
			}

			contentTask
		}

		/**
			* creates a folder that can have content files placed into it, the content task will copy them
			*/
		def contentProduce(classifier: String): File =
			findContentTask(classifier) / task.getName

		def contentReadout(classifier: String): Later[File] = {

			val List(contentTask: Content.Task) =
				task.getProject.findTasks[Content.Task]

			task.perform {
				contentTask.contentOutput
			}
		}

		private def findContentTask: Content.Task =
			task.getProject.getTasks.filterTo[Content.Task] match {
				case Nil =>
					task.getProject.install[Content.Task]
					findContentTask

				case List(contentTask: Content.Task) =>
					contentTask
			}
	}

}
