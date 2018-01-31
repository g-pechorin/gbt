package peterlavalle

import java.util


class RunnableFuture extends Runnable {
	private var list: Option[util.List[Runnable]] = Some(new util.LinkedList[Runnable]())
	private var locked: Boolean = false

	def !(lambda: => Unit): Unit =
		add(
			new Runnable {
				require(!locked, "no can add tasks after locked!")

				override def run(): Unit = lambda
			}
		)

	def add(runnable: Runnable): Unit = {
		require(!locked, "no can add tasks after locked!")
		list.get.add(runnable)
	}

	override def run(): Unit = {
		if (!locked)
			lock()
		require(locked)
		for (task <- list.get)
			task.run()

		list = None
	}

	def lock(): Unit = {
		require(!locked, "this should only be locked once")
		locked = true
	}
}
