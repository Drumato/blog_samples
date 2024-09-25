CREATE TABLE sample1 (
    id INT AUTO_INCREMENT PRIMARY KEY,
    contents VARCHAR(50) NOT NULL
);

CREATE TABLE sample2 (
    id INT AUTO_INCREMENT PRIMARY KEY,
    sample1_id INT,
    FOREIGN KEY (sample1_id) REFERENCES sample1(id)
);

CREATE TABLE sample3 (
    id INT AUTO_INCREMENT PRIMARY KEY,
    sample2_id INT,
    FOREIGN KEY (sample2_id) REFERENCES sample2(id)
);

CREATE TABLE sample4 (
    id INT AUTO_INCREMENT PRIMARY KEY,
    sample3_id INT,
    FOREIGN KEY (sample3_id) REFERENCES sample3(id)
);

CREATE TABLE sample5 (
    id INT AUTO_INCREMENT PRIMARY KEY,
    sample5_id INT,
    FOREIGN KEY (sample5_id) REFERENCES sample5(id)
);
