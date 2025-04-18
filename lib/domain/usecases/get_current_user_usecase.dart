import 'package:dartz/dartz.dart';
import 'package:equatable/equatable.dart';
import 'package:template_project/core/error/failures.dart';
import 'package:template_project/domain/entities/user.dart';
import 'package:template_project/domain/repositories/auth_repository.dart';

class GetCurrentUserUseCase {
  final AuthRepository repository;

  GetCurrentUserUseCase(this.repository);

  Future<Either<Failure, User?>> call(NoParams params) async {
    return await repository.getCurrentUser();
  }
}

class NoParams extends Equatable {
  @override
  List<Object> get props => [];
}
