package peterlavalle.gbt

trait TTestProjectAssembles extends TIntegrationTest {

	def testProjectAssembles(): Unit =
		gradleGoals(rootProject / testPath, "clean", "assemble")


}
