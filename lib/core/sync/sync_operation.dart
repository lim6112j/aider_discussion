class SyncOperation {
  final String type;
  final Map<String, dynamic> data;
  final DateTime createdAt;
  
  SyncOperation({
    required this.type,
    required this.data,
    DateTime? createdAt,
  }) : createdAt = createdAt ?? DateTime.now();
  
  factory SyncOperation.fromJson(Map<String, dynamic> json) {
    return SyncOperation(
      type: json['type'],
      data: Map<String, dynamic>.from(json['data']),
      createdAt: DateTime.parse(json['createdAt']),
    );
  }
  
  Map<String, dynamic> toJson() {
    return {
      'type': type,
      'data': data,
      'createdAt': createdAt.toIso8601String(),
    };
  }
}
