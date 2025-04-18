import 'package:equatable/equatable.dart';

abstract class Failure extends Equatable {
  final List properties;

  const Failure([this.properties = const <dynamic>[]]);

  @override
  List<Object> get props => [properties];
}

// General failures
class ServerFailure extends Failure {
  const ServerFailure();
}

class CacheFailure extends Failure {
  const CacheFailure();
}

class NetworkFailure extends Failure {
  const NetworkFailure();
}

class OfflineQueuedFailure extends Failure {
  const OfflineQueuedFailure();
}

class AuthenticationFailure extends Failure {
  final String message;
  
  const AuthenticationFailure({this.message = 'Authentication failed'});
  
  @override
  List<Object> get props => [message];
}
