package peterlavalle.gbt

import java.io.File

import peterlavalle.Later
import peterlavalle.gbt.TProperTask.{Consumption, Source}

class DummyProperTask extends TProperTask.TTaskPhased("who?", "what?") {

	val java: Later[List[(File, String)]] =
		consume("java", Consumption.Full)

	val binary: Later[Nothing] =
		produce("class") {

			java ? {
				sources: List[Source] =>
					???
			}
		}

}
