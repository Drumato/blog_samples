import mysql.connector
import time
import numpy as np
import matplotlib.pyplot as plt

MYSQL_USER = 'root'
MYSQL_PASSWORD = 'rootpassword'
MYSQL_HOST = 'localhost'
MYSQL_DATABASE = 'testdb'

connection = mysql.connector.connect(
    host=MYSQL_HOST,
    user=MYSQL_USER,
    password=MYSQL_PASSWORD,
    database=MYSQL_DATABASE
)

cursor = connection.cursor()

def measure_query_time(query, repeat_count=10):
    times = []
    for _ in range(repeat_count):
        start_time = time.time()
        cursor.execute(query)
        cursor.fetchall()
        end_time = time.time()
        times.append(end_time - start_time)
    return np.array(times)

queries = {
    "JOIN 2 tables": """
        SELECT sample1.id, sample2.id
        FROM sample1
        JOIN sample2 ON sample1.id = sample2.sample1_id;
    """,
    "JOIN 3 tables": """
        SELECT sample1.id, sample2.id, sample3.id
        FROM sample1
        JOIN sample2 ON sample1.id = sample2.sample1_id
        JOIN sample3 ON sample2.id = sample3.sample2_id;
    """,
    "JOIN 4 tables": """
        SELECT sample1.id, sample2.id, sample3.id, sample4.id
        FROM sample1
        JOIN sample2 ON sample1.id = sample2.sample1_id
        JOIN sample3 ON sample2.id = sample3.sample2_id
        JOIN sample4 ON sample3.id = sample4.sample3_id;
    """,
    "JOIN 5 tables": """
        SELECT sample1.id, sample2.id, sample3.id, sample4.id, sample5.id
        FROM sample1
        JOIN sample2 ON sample1.id = sample2.sample1_id
        JOIN sample3 ON sample2.id = sample3.sample2_id
        JOIN sample4 ON sample3.id = sample4.sample3_id
        JOIN sample5 ON sample4.id = sample5.sample5_id;
    """
}

repeat_count = 10

execution_stats = {}

for description, query in queries.items():
    print(f"Executing: {description}")
    times = measure_query_time(query, repeat_count)
    avg_time = np.mean(times)
    p99_time = np.percentile(times, 99)
    execution_stats[description] = {
        "avg_time": avg_time,
        "p99_time": p99_time
    }
    print(f"Average time: {avg_time:.6f} seconds, P99 time: {p99_time:.6f} seconds")

cursor.close()
connection.close()

descriptions = list(execution_stats.keys())
avg_times = [execution_stats[desc]["avg_time"] for desc in descriptions]
p99_times = [execution_stats[desc]["p99_time"] for desc in descriptions]

plt.figure(figsize=(10, 6))

plt.bar(descriptions, avg_times, color='blue', alpha=0.7, label='Average Time (s)')
plt.plot(descriptions, p99_times, color='red', marker='o', linestyle='-', label='P99 Time (s)')

plt.xlabel('Query Type')
plt.ylabel('Execution Time (seconds)')
plt.title('Execution Time (Average and P99) of JOIN Queries with 2, 3, 4, 5 Tables')
plt.xticks(rotation=45)
plt.legend()
plt.tight_layout()

plt.savefig('join_query_times.png')

plt.show()

