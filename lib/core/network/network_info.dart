import 'package:template_project/core/network/connectivity_service.dart';

abstract class NetworkInfo {
  Future<bool> get isConnected;
  Stream<bool> get connectionStatus;
}

class NetworkInfoImpl implements NetworkInfo {
  final ConnectivityService connectivityService;

  NetworkInfoImpl(this.connectivityService);

  @override
  Future<bool> get isConnected => connectivityService.isConnected;
  
  @override
  Stream<bool> get connectionStatus => connectivityService.connectionStatus;
}
