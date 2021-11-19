library(nomnoml)

nomnoml("
[<frame>Iteration U|
        [<input>μ] --> [n(U)]
        [n(U)]number of -> [<actor>agents]
        [<actor>agents] - [q questions to answer]
        [<input>p] --> [<actor>% of cheaters within]
        [<actor>% of cheaters within] -> [<actor>agents]
        [<input>μ] --> +[T(U)]
        [<input>T(U)] --> [t]
        [t]True Score of -> [<actor>agents]
]",
        svg = TRUE)
        
nomnoml("
[<actor>Other agents] --> [X]

[<actor>Alice|[t(Alice)]] --> knows [x(t(Alice))]
[x(t(Alice))] -> [<state>summed into x(Alice)]
[<actor>Alice] - [if Alice; is cheater]
[if Alice; is cheater] --> [x(c(Alice))]
[x(c(Alice))] -> [<state>summed into x(Alice)]
[x(e(Alice))].25 -> [<state>summed into x(Alice)]
[x(t(Alice))] --> -[x(c(Alice))]
[<state>summed into x(Alice)] --> [X]

[<actor>Bob|[t(Bob)]] --> knows [x(t(Bob))]
[x(t(Bob))] -> [<state>summed into x(Bob)]
[<actor>Bob] - [if Bob; is cheater]
[if Bob; is cheater] --> [x(c(Bob))]
[x(c(Bob))] --> [<state>summed into x(Bob)]
[x(e(Bob))].25 -> [<state>summed into x(Bob)]
[x(t(Bob))] --> -[x(c(Bob))]
[<state>summed into x(Bob)] --> [X]

[X] --> [<database>Ranking]
",
        svg = TRUE)
