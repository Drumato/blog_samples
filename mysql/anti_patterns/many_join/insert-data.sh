#!/bin/bash

MYSQL_USER="root"
MYSQL_PASSWORD="rootpassword"
MYSQL_DATABASE="testdb"

LOOP_COUNT=1000

MYSQL_CMD="docker exec mysql mysql -u$MYSQL_USER -p$MYSQL_PASSWORD -D$MYSQL_DATABASE --silent --skip-column-names"

echo "Inserting into sample1..."
for i in $(seq 1 $LOOP_COUNT); do
    QUERY="INSERT INTO sample1 (contents) VALUES ('Content $i');"
    echo "Executing: $QUERY"
    $MYSQL_CMD -e "$QUERY" 2>/dev/null
done

echo "Inserting into sample2..."
for i in $(seq 1 $LOOP_COUNT); do
    QUERY="INSERT INTO sample2 (sample1_id) VALUES ($i);"
    echo "Executing: $QUERY"
    $MYSQL_CMD -e "$QUERY" 2>/dev/null
done

echo "Inserting into sample3..."
for i in $(seq 1 $LOOP_COUNT); do
    QUERY="INSERT INTO sample3 (sample2_id) VALUES ($i);"
    echo "Executing: $QUERY"
    $MYSQL_CMD -e "$QUERY" 2>/dev/null
done

echo "Inserting into sample4..."
for i in $(seq 1 $LOOP_COUNT); do
    QUERY="INSERT INTO sample4 (sample3_id) VALUES ($i);"
    echo "Executing: $QUERY"
    $MYSQL_CMD -e "$QUERY" 2>/dev/null
done

echo "Inserting into sample5..."
for i in $(seq 1 $LOOP_COUNT); do
    QUERY="INSERT INTO sample5 (sample5_id) VALUES ($i);"
    echo "Executing: $QUERY"
    $MYSQL_CMD -e "$QUERY" 2>/dev/null
done

echo "Data insertion completed."

