import 'dart:async';
import 'dart:convert';
import 'package:shared_preferences/shared_preferences.dart';
import 'package:template_project/core/network/network_info.dart';
import 'package:template_project/core/sync/sync_operation.dart';

class SyncManager {
  final NetworkInfo networkInfo;
  final SharedPreferences sharedPreferences;
  final Map<String, SyncOperationHandler> _handlers = {};
  StreamSubscription? _connectivitySubscription;
  
  static const String _queueKey = 'SYNC_OPERATION_QUEUE';
  
  SyncManager({
    required this.networkInfo,
    required this.sharedPreferences,
  }) {
    // Listen for connectivity changes
    _connectivitySubscription = networkInfo.connectionStatus.listen((isConnected) {
      if (isConnected) {
        processQueue();
      }
    });
  }
  
  // Register handlers for different operation types
  void registerHandler(String operationType, SyncOperationHandler handler) {
    _handlers[operationType] = handler;
  }
  
  // Queue an operation for later execution
  Future<void> queueOperation(SyncOperation operation) async {
    List<String> queue = _getQueue();
    queue.add(jsonEncode(operation.toJson()));
    await sharedPreferences.setStringList(_queueKey, queue);
  }
  
  // Get the current operation queue
  List<String> _getQueue() {
    return sharedPreferences.getStringList(_queueKey) ?? [];
  }
  
  // Process all queued operations
  Future<void> processQueue() async {
    if (!(await networkInfo.isConnected)) {
      return;
    }
    
    List<String> queue = _getQueue();
    if (queue.isEmpty) {
      return;
    }
    
    List<String> remainingOperations = [];
    
    for (String operationJson in queue) {
      try {
        final operation = SyncOperation.fromJson(jsonDecode(operationJson));
        final handler = _handlers[operation.type];
        
        if (handler != null) {
          final success = await handler(operation.data);
          if (!success) {
            // If operation failed, keep it in the queue
            remainingOperations.add(operationJson);
          }
        } else {
          // No handler found, keep in queue
          remainingOperations.add(operationJson);
        }
      } catch (e) {
        // Error processing this operation, keep it in queue
        remainingOperations.add(operationJson);
      }
    }
    
    // Update the queue with remaining operations
    await sharedPreferences.setStringList(_queueKey, remainingOperations);
  }
  
  // Manually trigger queue processing
  Future<void> sync() async {
    return processQueue();
  }
  
  void dispose() {
    _connectivitySubscription?.cancel();
  }
}

// Type definition for operation handlers
typedef SyncOperationHandler = Future<bool> Function(Map<String, dynamic> data);
