import 'package:dartz/dartz.dart';
import 'package:template_project/core/error/exceptions.dart';
import 'package:template_project/core/error/failures.dart';
import 'package:template_project/core/network/network_info.dart';
import 'package:template_project/data/datasources/local/auth_local_data_source.dart';
import 'package:template_project/data/datasources/remote/auth_remote_data_source.dart';
import 'package:template_project/domain/entities/user.dart';
import 'package:template_project/domain/repositories/auth_repository.dart';

class AuthRepositoryImpl implements AuthRepository {
  final AuthRemoteDataSource remoteDataSource;
  final AuthLocalDataSource localDataSource;
  final NetworkInfo networkInfo;

  AuthRepositoryImpl({
    required this.remoteDataSource,
    required this.localDataSource,
    required this.networkInfo,
  });

  @override
  Future<Either<Failure, User>> login(String email, String password) async {
    if (await networkInfo.isConnected) {
      try {
        final user = await remoteDataSource.login(email, password);
        await localDataSource.cacheUser(user);
        return Right(user);
      } on ServerException {
        return const Left(ServerFailure());
      } on AuthenticationException catch (e) {
        return Left(AuthenticationFailure(message: e.message));
      }
    } else {
      return const Left(NetworkFailure());
    }
  }

  @override
  Future<Either<Failure, User>> register(String name, String email, String password) async {
    if (await networkInfo.isConnected) {
      try {
        final user = await remoteDataSource.register(name, email, password);
        await localDataSource.cacheUser(user);
        return Right(user);
      } on ServerException {
        return const Left(ServerFailure());
      } on AuthenticationException catch (e) {
        return Left(AuthenticationFailure(message: e.message));
      }
    } else {
      return const Left(NetworkFailure());
    }
  }

  @override
  Future<Either<Failure, bool>> logout() async {
    try {
      await localDataSource.clearUser();
      return const Right(true);
    } on CacheException {
      return const Left(CacheFailure());
    }
  }

  @override
  Future<Either<Failure, User?>> getCurrentUser() async {
    try {
      final user = await localDataSource.getLastUser();
      return Right(user);
    } on CacheException {
      return const Left(CacheFailure());
    }
  }
}
