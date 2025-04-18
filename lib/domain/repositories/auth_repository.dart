import 'package:dartz/dartz.dart';
import 'package:template_project/core/error/failures.dart';
import 'package:template_project/domain/entities/user.dart';

abstract class AuthRepository {
  Future<Either<Failure, User>> login(String email, String password);
  Future<Either<Failure, User>> register(String name, String email, String password);
  Future<Either<Failure, bool>> logout();
  Future<Either<Failure, User?>> getCurrentUser();
}
