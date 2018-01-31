package peterlavalle.gbt

trait TTestTasks extends TIntegrationTest {

	def testITTasks(): Unit =
		gradleGoals(rootProject / testPath, "clean", "tasks")

}
