(extend-protocol Inst
  java.util.Calendar
  (inst-ms* [inst] (.getTimeInMillis ^java.util.Calendar inst)))
