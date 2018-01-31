package peterlavalle.gbt

trait TTestProjectPassesTests extends TIntegrationTest {
	def testProjectPassesTests(): Unit =
		gradleGoals(rootProject / testPath, "clean", "test")
}
