package peterlavalle.gbt

import javax.script.{ScriptEngine, ScriptEngineManager}

import org.gradle.api._
import org.gradle.api.file.SourceDirectorySet
import org.gradle.api.internal.file.SourceDirectorySetFactory
import org.gradle.api.plugins.ExtensionContainer
import peterlavalle.{LazyCache, RunnableFuture}

import scala.beans.BeanProperty
import scala.reflect.ClassTag

object APlugin {

	type SpawnSourceCode = (String, SourceDirectorySet) => APlugin.TSourceSet
	val spawnCache: LazyCache[String, SpawnSourceCode] = {
		val scriptEngine: ScriptEngine = new ScriptEngineManager().getEngineByName("groovy")
		LazyCache[String, SpawnSourceCode] {
			kind: String =>
				scriptEngine.eval(
					s"""
						 |import org.gradle.api.file.SourceDirectorySet
						 |import org.gradle.util.ConfigureUtil
						 |import peterlavalle.gbt.APlugin
						 |import scala.Function2
						 |
						 |new Function2<String, SourceDirectorySet, APlugin.TSourceSet>() {
						 |	@Override
						 |	APlugin.TSourceSet apply(String d, SourceDirectorySet s) {
						 |		return new APlugin.TSourceSet() {
						 |			final String displayName = d
						 |
						 |			String displayName() { return d; }
						 |			final SourceDirectorySet src = s
						 |
						 |			SourceDirectorySet src() { return s; }
						 |			final SourceDirectorySet $kind = s
						 |
						 |			SourceDirectorySet $kind() { return s; }
						 |
						 |			APlugin.TSourceSet $kind(Closure<?> configureClosure) {
						 |				ConfigureUtil.configure(configureClosure, s);
						 |				return this;
						 |			}
						 |		}
						 |	}
						 |}
					""".stripMargin
				).asInstanceOf[(String, SourceDirectorySet) => APlugin.TSourceSet]
		}
	}

	def upName(name: String): String =
		s"${name.substring(0, 1).toUpperCase()}${name.substring(1)}"

	trait TSourceSet {
		val displayName: String
		val src: SourceDirectorySet

		final def Name: String = upName(name)

		@BeanProperty
		final def name: String = displayName
	}

}

/**
	* base class for plugins. this (basically) exists to ease the creation of source sets
	*/
abstract class APlugin(val sourceDirectorySetFactory: SourceDirectorySetFactory)
	extends org.gradle.api.Plugin[Project]
		with TPackage
		with MSources.MPlugin {

	private val configureRunnables: RunnableFuture = new RunnableFuture()
	private var target: Option[Project] = None

	final override def apply(target: Project): Unit = {

		//
		// check that we're not doing anything trickey
		val extName: String = getClass.getPackage.getName.split("\\.").last
		requyre[GradleException](
			extName != target.getName,
			s"Can't name the project $extName"
		)
		target.getTasks.foreach {
			task: Task =>
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

	final def install[T <: TProperTask](implicit classTag: ClassTag[T]): Unit =
		configure {
			TProject.Gradle(project).install[T]
		}

	final def project: Project =
		target match {
			case Some(project: Project) =>
				project
		}

	final def configure(lambda: => Unit): Unit =
		configureRunnables.add(() => lambda)

	final def plugin[P <: Object](implicit classTag: ClassTag[P]): Unit =
		configure {
			project.getPluginManager.apply(classTag.runtimeClass)
		}
}
