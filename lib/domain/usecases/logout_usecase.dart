import 'package:dartz/dartz.dart';
import 'package:template_project/core/error/failures.dart';
import 'package:template_project/domain/repositories/auth_repository.dart';
import 'package:template_project/domain/usecases/get_current_user_usecase.dart';

class LogoutUseCase {
  final AuthRepository repository;

  LogoutUseCase(this.repository);

  Future<Either<Failure, bool>> call(NoParams params) async {
    return await repository.logout();
  }
}
