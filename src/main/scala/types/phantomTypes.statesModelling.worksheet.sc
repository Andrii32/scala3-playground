// ====================
{
  trait TaskState
  case class PENDING() extends TaskState
  case class RECEIVED() extends TaskState
  case class STARTED() extends TaskState

  class TaskStateException(message: String) extends Exception(message)
  case class Task(state: TaskState) {
    def take = state match {
      case _: PENDING => Task(RECEIVED())
      case _ => throw new TaskStateException("bad state")
    }

    def execute = state match {
      case _: RECEIVED => Task(STARTED())
      case _ => throw new TaskStateException("bad state")
    }
    // ...
  }
}

// ====================
// states modelling using inheritance

{
  trait TaskState
  case class PENDING() extends TaskState {
    def take = new RECEIVED()
  }
  case class RECEIVED() extends TaskState {
    def execute = new STARTED()
  }
  case class STARTED() extends TaskState {
    def success = new SUCCESS()
    def failure = new FAILURE()
  }
  case class SUCCESS() extends TaskState {
    def restart = new PENDING()
  }
  case class FAILURE() extends TaskState {
    def restart = new PENDING()
  }
}

// ====================
// states modelling using phantom types

sealed trait TaskState
sealed trait PENDING extends TaskState
sealed trait RECEIVED extends TaskState
sealed trait STARTED extends TaskState
sealed trait SUCCESS extends TaskState
sealed trait FAILURE extends TaskState


// using phantom types
case class Task[S <: TaskState](){
  def take(implicit ev: S =:= PENDING) = Task[RECEIVED]()
  def execute(implicit ev: S =:= RECEIVED) = Task[STARTED]()
  def success(implicit ev: S =:= STARTED) = Task[SUCCESS]()
  def failure(implicit ev: S =:= STARTED) = Task[FAILURE]()
  def restart(implicit ev: S =:= SUCCESS | S =:= FAILURE) = Task[PENDING]()
}


// Task[SUCCESS]().take // cannot prove that SUCCESS =:= PENDING
Task[SUCCESS]().restart

// ====================

