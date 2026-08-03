# Briefing design note

Why this deck is built the way it is. Written for the presenter, and for whoever inherits the deck when the account changes hands.

## Who the audience is, and what follows from that

Public opinion analysts, mostly political science backgrounds. They are fluent in survey results and in substantive interpretation. They are not fluent in complex sample design beyond weighting, and they have little exposure to psychometrics. Two consequences drive every structural choice below.

First, a methods framing loses them in the opening seconds. Anything that begins with a framework diagram, an equation, or "as you know, complex surveys" signals a lecture and spends attention that never comes back. So the deck opens on a measurement problem they already have, not on a method that solves it.

Second, they will not accept a result they cannot connect to something they produce. So the findings arrive as consequences for a country account, and the mechanism arrives afterward as the explanation for a question they now want answered.

## The arc

Five movements, in this order, for these reasons.

**Slides 1 to 3, the problem.** The hook is the measurement gap: we report on concepts no respondent was ever asked about, and the step from a battery of items to a population statement has always been made by convention. This is provocative to this audience precisely because they do it constantly and have never had to defend it. Slide 3 states the fork the whole project turns on, degree versus kind, using schematic pictures and no data, so the distinction lands before any real numbers can complicate it.

The earlier draft opened with two respondents who share a score, and that was cut. It assumed the audience could read a response vector and see the problem in it. This audience cannot, or at least should not be asked to in the first thirty seconds.

**Slide 4, the roadmap.** Deliberately fourth, not first. A roadmap given before the question exists is a table of contents nobody reads. Given after slides 1 to 3, it organizes a question the room now has.

**Slides 5 to 8, what we found.** Segments, then population shares with intervals, then the demographic breakdown, then the three-estimator comparison. The ordering moves from most concrete to most consequential. Slide 6 exists to make one word land, population: a cluster analysis returns groups in a file, this returns shares of a country with uncertainty attached. Slide 7 is their standard cross-tab showing something their standard cross-tab cannot, which is the moment the method becomes theirs rather than mine. Slide 8 is the payoff for the whole design argument, and it is framed entirely as consequences, never as estimator theory.

**Slides 9 to 13, how we got there.** The model, then the k-means comparison, then the sample design, then the uncertainty result, then the AI layer. Each answers a question the findings raised rather than preempting one.

Slide 10 answers "why not the clustering I already know," which is the second question in every head in the room and is better answered early than fielded from the floor.

Slide 11 carries the total survey error content without the framework. Four plain phrases naming which error sources this addresses: sampling error estimated from the realized design, measurement error made a parameter, item nonresponse handled by scoring partial responders, classification error introduced and then corrected. The framework diagram is not on screen because for this audience the diagram is the part that loses them, and the four phrases are the part that persuades them.

Slide 12 volunteers the least flattering result in the project: propagating the measurement model roughly doubles the margins of error. This is on the critical path rather than in backup on purpose. An audience decides whether to trust a methods talk by whether the presenter names the weaknesses before being asked, and this one is both real and consequential for how findings should be reported.

Slide 13 places the AI use where it is defensible rather than where it is impressive. Late, after the numbers are believed, framed as constraints imposed rather than limitations discovered, and explicit that names vary between runs and never enter a calculation. Leading with AI would have made the novelty carry the talk and left the statistics looking like scaffolding.

**Slides 14 to 15, the ask.** Portability and collaboration, then the knowledge-capture question that is the actual purpose of the briefing. Fifteen is the only slide with a decision attached and it holds three minutes of discussion, which is why nothing else is allowed to borrow from it.

## The central question being asked

The details cannot go into a policymaker product. A two-page product carries a segment name and a share; it cannot carry the design, the corrections, or the caveats. The briefing asks the room to choose among a standing methods annex, a shared archive of configurations and diagnostics, and a one-pager that travels with each product, and to answer three specific questions: what must be preserved to reproduce a published number in two years, who reviews a segment name before it reaches a briefing, and what the minimum caveat is that must appear in the product itself.

This is the reason for the briefing. Everything before it exists to earn the right to ask.

## Conventions in the deck

Every slide title is an assertion, not a topic. The line immediately under each title is the bottom line for that slide, stating what is happening and why it matters, so a reader who sees only titles and bottom lines gets the argument.

Slides are tagged `[CORE]`, `[OPTIONAL]` or `[BACKUP]` in HTML comments. Fifteen minutes is core only. First cut is the k-means slide, and only if nobody in the room does cluster analysis. Second cut merges the model slide into the design slide.

Backup slides answer the questions a methods-literate attendee will raise: how the correction works, how the number of segments was chosen, which items are carrying the analysis, and the validation against an independent implementation. Answering from a backup slide reads as preparation; improvising reads as digression.

Speaker notes carry the intended pacing and the one thing to say on each slide. They project on a second screen if the `setbeameroption` line in the header is uncommented.

## What must be changed per account

The `params` block at the top holds the study name, the concept, the sample counts, the featured demographic, and the two step-one sensitivity figures. The classification banner is a single command in the LaTeX header.

Two items in the deck are not parameterized and must be edited by hand. The four item labels in the slide 2 diagram are hardcoded and should name items from the account being briefed, since that slide is the hook and generic labels blunt it. The `step1_median` and `step1_max` params are carried from the demonstration run and must be replaced with the figures from the account's own sensitivity refit, or the slide reports someone else's number.

The deck reads one artifact, `model_fit.rds`, plus the segment label and demographic CSVs the report writes. It does not read parquet files, and it must not be pointed at a different artifact contract than the report produces, or the two will drift.
