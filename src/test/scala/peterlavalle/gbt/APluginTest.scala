package peterlavalle.gbt

import org.easymock.EasyMock
import org.junit.Assert._
import peterlavalle.{ATestCase, LazyCache}

class APluginTest extends ATestCase {


	def testCacheMagic(): Unit = {
		val factory = newMock[String => Int]

		factory.mockExpect(_ ("one"), 1).once()
		factory.mockExpect(_ ("two"), 2).once()

		EasyMock.replay(factory)


		val cache = LazyCache[String, Int](factory)


		assertEquals(1, cache("one"))
		assertEquals(2, cache("two"))

		(0 until 29).foreach {
			_ =>

				assertEquals(1, cache("one"))
				assertEquals(2, cache("two"))
		}

		EasyMock.verify(factory)
	}

}
