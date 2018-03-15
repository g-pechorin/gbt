package peterlavalle.gbt

import org.gradle.api.GradleException

trait TDiagnostics extends peterlavalle.TDiagnostics {

	def GradleFail(message: String): Nothing =
		throw new GradleException(message)

}
