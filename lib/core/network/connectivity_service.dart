import 'dart:async';
import 'package:connectivity_plus/connectivity_plus.dart';

class ConnectivityService {
  final Connectivity connectivity;
  
  // Stream controller for connectivity status
  final _connectionStatusController = StreamController<bool>.broadcast();
  
  // Stream that emits connectivity status
  Stream<bool> get connectionStatus => _connectionStatusController.stream;
  
  ConnectivityService(this.connectivity) {
    // Initialize with current status
    connectivity.checkConnectivity().then((result) {
      _connectionStatusController.add(result != ConnectivityResult.none);
    });
    
    // Listen for connectivity changes
    connectivity.onConnectivityChanged.listen((ConnectivityResult result) {
      _connectionStatusController.add(result != ConnectivityResult.none);
    });
  }
  
  // Check current connectivity
  Future<bool> get isConnected async {
    final connectivityResult = await connectivity.checkConnectivity();
    return connectivityResult != ConnectivityResult.none;
  }
  
  void dispose() {
    _connectionStatusController.close();
  }
}
