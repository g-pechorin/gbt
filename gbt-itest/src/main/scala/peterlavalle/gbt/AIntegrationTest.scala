package peterlavalle.gbt

/**
	* Stupid hack to *DO* integration-tests as part of unit-testing
	* - runs `gradlew assemble` on the "root" project
	* - runs `gradlew test` on the path'ed project
	*/
abstract class AIntegrationTest
	extends TIntegrationTest
		with TTestPresent
		with TTestProjectAssembles
		with TTestProjectPassesTests
		with TTestTasks
