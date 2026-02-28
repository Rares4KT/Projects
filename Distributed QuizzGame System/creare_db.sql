CREATE TABLE intrebari (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    text TEXT NOT NULL,
    vA TEXT NOT NULL,
    vB TEXT NOT NULL,
    vC TEXT NOT NULL,
    corect TEXT NOT NULL
);

INSERT INTO intrebari (text, vA, vB, vC, corect) VALUES 
('Care este capitala Frantei?', 'Londra', 'Paris', 'Berlin', 'B');

INSERT INTO intrebari (text, vA, vB, vC, corect) VALUES 
('Cati biti are un byte?', '8', '16', '4', 'A');

INSERT INTO intrebari (text, vA, vB, vC, corect) VALUES 
('Cine a scris Luceafarul?', 'Ion Creanga', 'Mihai Eminescu', 'George Bacovia', 'B');
