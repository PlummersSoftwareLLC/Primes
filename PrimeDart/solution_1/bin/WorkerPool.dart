import 'dart:async';
import 'dart:isolate';

/// Messages we can send to the primeWorker to process.
abstract class Message {
  const Message();
}

/// Indicates the worker should start running.
class Start extends Message {
  final void Function() work;
  const Start({required this.work});
}

/// Indicates the worker should stop running.
class Stop extends Message {
  const Stop();
}

/// Indicates the worker should create another prime sieve.
class Continue extends Message {
  const Continue();
}

/// In dart, you do not have direct access to threads, instead dart
/// provides a construct called an isolate. Each isolate is it's own
/// isolated execution context with it's own memory and it's own event
/// loop. The isolates can only communicate to each other by
/// sending/recieving messages using SendPort and ReceivePort objects.
///
/// This WorkerPool class initializes multiple isolates to work on a
/// particular task. When a Start(work) message is broadcast to the
/// pool, each isolate in the pool will perform the given work task
/// repeatedly until it receives a Stop() message.
class WorkerPool {
  final int numberOfWorkers;
  final List<SendPort> _sendPorts = [];
  final List<Future<int>> _responses = [];

  /// This is the handler function that will be spawned on each
  /// isolate.
  ///
  /// When the handler receives a Start() message it extracts the
  /// callback function which is used to perform the work task.
  /// Then the handler sends a Continue() message to itself to
  /// begin the work.
  ///
  /// When the handler receives a Stop() message it sends the
  /// total count of the amount of times it ran the work callback
  /// back to the main isolate.
  ///
  /// When the handler receives a Continue() message it calls the
  /// work callback, increments the count, and then it sends
  /// another Continue() message to itself.
  static Future<void> _worker(SendPort sendPort) async {
    final receivePort = ReceivePort();
    sendPort.send(receivePort.sendPort);
    var count = 0;
    late void Function() work;

    await for (final message in receivePort) {
      switch (message.runtimeType) {
        case Start:
          work = (message as Start).work;
          receivePort.sendPort.send(const Continue());
          break;
        case Stop:
          sendPort.send(count);
          receivePort.close();
          break;
        case Continue:
          work();
          count++;
          receivePort.sendPort.send(const Continue());
          break;
      }
    }
  }

  /// Private constructor, use init instead to instantiate a WorkerPool.
  WorkerPool._(this.numberOfWorkers);

  /// Spawns [numberOfWorkers] amount of isolates. Also initializes the
  /// List<SendPort> _sendPorts which are used to broadcast messages to
  /// the isolates, and also the List<Future<int>> _responses which will
  /// complete after a Stop() message is broadcast to all isolates.
  static Future<WorkerPool> init({required int numberOfWorkers}) async {
    final pool = WorkerPool._(numberOfWorkers);

    for (var i = 0; i < numberOfWorkers; i++) {
      final receivePort = ReceivePort();
      await Isolate.spawn(_worker, receivePort.sendPort);
      final messages = receivePort.take(2).asBroadcastStream();
      final sendPort = await messages.first as SendPort;
      pool._sendPorts.add(sendPort);
      pool._responses.add(messages.last.then((value) => value as int));
    }

    return pool;
  }

  // Sends the same message to every isolate in the pool.
  void broadcast(Message message) {
    for (var i = 0; i < numberOfWorkers; i++) {
      _sendPorts[i].send(message);
    }
  }

  // Gets the combined number of passes from all isolates.
  // Note that the Future will not complete until a Stop()
  // message has been broadcast to all isolates.
  Future<int> passes() =>
      Stream.fromFutures(_responses).fold(0, (a, b) => a + b);
}
