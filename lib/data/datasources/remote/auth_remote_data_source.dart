import 'package:dio/dio.dart';
import 'package:template_project/core/error/exceptions.dart';
import 'package:template_project/data/models/user_model.dart';

abstract class AuthRemoteDataSource {
  Future<UserModel> login(String email, String password);
  Future<UserModel> register(String name, String email, String password);
}

class AuthRemoteDataSourceImpl implements AuthRemoteDataSource {
  final Dio dio;
  final String baseUrl;

  AuthRemoteDataSourceImpl({
    required this.dio,
    required this.baseUrl,
  });

  @override
  Future<UserModel> login(String email, String password) async {
    try {
      final response = await dio.post(
        '$baseUrl/auth/login',
        data: {
          'email': email,
          'password': password,
        },
      );

      if (response.statusCode == 200) {
        return UserModel.fromJson(response.data['user']);
      } else {
        throw AuthenticationException(
          message: response.data['message'] ?? 'Authentication failed',
        );
      }
    } on DioException catch (e) {
      if (e.response?.statusCode == 401) {
        throw AuthenticationException(
          message: e.response?.data['message'] ?? 'Invalid credentials',
        );
      }
      throw ServerException(message: e.message);
    }
  }

  @override
  Future<UserModel> register(String name, String email, String password) async {
    try {
      final response = await dio.post(
        '$baseUrl/auth/register',
        data: {
          'name': name,
          'email': email,
          'password': password,
        },
      );

      if (response.statusCode == 201) {
        return UserModel.fromJson(response.data['user']);
      } else {
        throw AuthenticationException(
          message: response.data['message'] ?? 'Registration failed',
        );
      }
    } on DioException catch (e) {
      if (e.response?.statusCode == 400) {
        throw AuthenticationException(
          message: e.response?.data['message'] ?? 'Registration failed',
        );
      }
      throw ServerException(message: e.message);
    }
  }
}
