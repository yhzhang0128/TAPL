/* Examples for testing */

/* Basics */
true;
lambda x.x;
(lambda x.x)(lambda x.x x);
lambda x. lambda y. lambda x.x;

/* Church Bool */
/* True */
lambda x. lambda y. x;
/* False */
lambda x. lambda y. y;

/* Bool Operation: And */
/* lambda x. lambda y. x y fls */
(lambda x. lambda y. x y (lambda x. lambda y. y));
(lambda x. lambda y. x y (lambda x. lambda y. y)) (lambda x. lambda y. x) (lambda x. lambda y. x);

/* Bool Operation: Or */
/* lambda x. lambda y. x tru y */
(lambda x. lambda y. x (lambda x. lambda y. x) y);
(lambda x. lambda y. x (lambda x. lambda y. x) y) (lambda x. lambda y. y) (lambda x. lambda y. y);

/* Dangerous: Omega combinator */
(lambda x.x x) (lambda x.x x);
