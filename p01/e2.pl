% teaches(Course, Teacher)
teaches(algorithms, adalberto).
teaches(databases, bernardete).
teaches(compilers, capitolino).
teaches(statistics, diógenes).
teaches(networks, ermelinda).

% attends(Course, Student)
attends(algorithms, alberto).
attends(algorithms, bruna).
attends(algorithms, cristina).
attends(algorithms, diogo).
attends(algorithms, eduarda).
attends(databases, antónio).
attends(databases, bruno).
attends(databases, cristina).
attends(databases, duarte).
attends(databases, eduardo).
attends(compilers, alberto).
attends(compilers, bernardo).
attends(compilers, clara).
attends(compilers, diana).
attends(compilers, eurico).
attends(statistics, antónio).
attends(statistics, bruna).
attends(statistics, cláudio).
attends(statistics, duarte).
attends(statistics, eva).
attends(networks, álvaro).
attends(networks, beatriz).
attends(networks, cláudio).
attends(networks, diana).
attends(networks, eduardo).

% student(Student, Professor)
student(Student, Professor) :-
        teaches(Course, Professor),
        attends(Course, Student).

% students(Professor)
students(Professor, Students) :-
        setof(Student, student(Student, Professor), Students).

% commonStudents(Professor1, Professor2, CommonStudents)
commonStudents(Professor1, Professor2, CommonStudents):-
        setof(Student,
              (student(Student, Professor1),
               student(Student, Professor2),
               Professor1 \== Professor2),
              CommonStudents).

% teachers(Student, Teachers)
teachers(Student, Teachers):-
        setof(Teacher, student(Student, Teacher), Teachers).

% colleagues(Person1, Person2)
colleagues(Person1, Person2):-
        % students
        attends(Course, Person1),
        attends(Course, Person2),
        Person1 \== Person2;
        % teachers
        teaches(_, Person1),
        teaches(_, Person2),
        Person1 \== Person2.

% multiCourseStudents(Students)
multiCourseStudents(Students) :-
    setof(Student,
          (attends(Course1, Student),
           attends(Course2, Student),
           Course1 \== Course2),
          Students).