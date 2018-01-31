package peterlavalle.gbt

import org.junit.Assert.assertTrue

trait TTestPresent extends TIntegrationTest {

	def testIsPresent(): Unit =
		assertTrue(
			(rootProject / s"$testPath/build.gradle").exists()
		)

}
