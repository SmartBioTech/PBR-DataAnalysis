-- DROP TABLE users, instruments, calibrations;
CREATE DATABASE IF NOT EXISTS Shiny_PBRDataAnalysis;
USE Shiny_PBRDataAnalysis;
CREATE TABLE IF NOT EXISTS users (
    user_id SMALLINT AUTO_INCREMENT,
    username VARCHAR(32) NOT NULL,
    passwd VARCHAR(32) NOT NULL,
    roles TINYINT NOT NULL,
    created_date TIMESTAMP  DEFAULT CURRENT_TIMESTAMP,
    access_date TIMESTAMP,
    PRIMARY KEY (user_id)
)  ENGINE=INNODB;

CREATE TABLE IF NOT EXISTS experiments (
    experiment_id SMALLINT AUTO_INCREMENT,
    user_id SMALLINT NOT NULL,
    created_date DATE,
    description TEXT,
    PRIMARY KEY (experiment_id)
)  ENGINE=INNODB;

CREATE TABLE IF NOT EXISTS instruments (
    instrument_id SMALLINT AUTO_INCREMENT,
    user_id SMALLINT NOT NULL,
    created_date TIMESTAMP  DEFAULT CURRENT_TIMESTAMP,
    description TEXT,
    PRIMARY KEY (instrument_id)
)  ENGINE=INNODB;

CREATE TABLE IF NOT EXISTS calibrations (
    calibration_id SMALLINT AUTO_INCREMENT,
    user_id SMALLINT NOT NULL,
    instrument_id SMALLINT NOT NULL,
    measured_date DATETIME NOT NULL,
    created_date TIMESTAMP  DEFAULT CURRENT_TIMESTAMP,
    slope FLOAT NOT NULL,
    intercept FLOAT NOT NULL,
    description TEXT,
    PRIMARY KEY (calibration_id)
)  ENGINE=INNODB;

INSERT INTO users (username, passwd, roles, access_date) VALUES ('cerveny.j', md5('Zamek37333'), 16, NOW());