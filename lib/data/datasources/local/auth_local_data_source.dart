import 'dart:convert';

import 'package:flutter_secure_storage/flutter_secure_storage.dart';
import 'package:template_project/core/error/exceptions.dart';
import 'package:template_project/data/models/user_model.dart';

abstract class AuthLocalDataSource {
  Future<UserModel?> getLastUser();
  Future<void> cacheUser(UserModel user);
  Future<void> clearUser();
}

class AuthLocalDataSourceImpl implements AuthLocalDataSource {
  final FlutterSecureStorage secureStorage;

  AuthLocalDataSourceImpl({required this.secureStorage});

  @override
  Future<UserModel?> getLastUser() async {
    try {
      final jsonString = await secureStorage.read(key: 'CACHED_USER');
      if (jsonString == null) {
        return null;
      }
      return UserModel.fromJson(json.decode(jsonString));
    } catch (e) {
      throw CacheException(message: 'Failed to retrieve cached user');
    }
  }

  @override
  Future<void> cacheUser(UserModel user) async {
    try {
      await secureStorage.write(
        key: 'CACHED_USER',
        value: json.encode(user.toJson()),
      );
    } catch (e) {
      throw CacheException(message: 'Failed to cache user');
    }
  }

  @override
  Future<void> clearUser() async {
    try {
      await secureStorage.delete(key: 'CACHED_USER');
    } catch (e) {
      throw CacheException(message: 'Failed to clear cached user');
    }
  }
}
