# Briefing design note

Why this deck is built the way it is. Written for the presenter, and for whoever inherits the deck when the account changes hands.

## Who the audience is, and what follows from that

Public opinion analysts, mostly political science backgrounds. They are fluent in survey results and in substantive interpretation. They are not fluent in complex sample design beyond weighting, and they have little exposure to psychometrics. Three consequences drive every structural choice below.

First, a methods framing loses them in the opening seconds. Anything that begins with a framework diagram, an equation, or "as you know, complex surveys" signals a lecture and spends attention that never comes back.

Second, they will not accept a result they cannot connect to something they produce. So the findings arrive as consequences for a country account, and the mechanism arrives afterward as the explanation for a question they now want answered.

Third, they read the screen while you talk. Any bullet that is a full sentence competes with the narration and loses both. Slides carry the shortest phrasing that still stands alone; the sentences live in the speaker notes.

## The hook

The opening is the measurement problem itself: we routinely report on concepts no respondent was ever asked about, and the step from a battery of items to a population statement has always been made by convention. That is provocative to this audience precisely because they do it constantly and have never had to defend it.

Two earlier hooks were tried and cut. Opening on two respondents who share a score assumed the audience could read a response vector and see the problem in it. Opening on a flat trend line assumed a specific case in the data that may not exist. Both put interpretive work on the room in the first thirty seconds. The measurement gap needs no graphic and no prior knowledge.

The slide 2 diagram runs its arrows from the concept down to the items, not upward. That is the measurement direction: the construct is what drives the answers, and each item is a fallible reading of it. Reversed arrows would say the items add up to the concept, which is exactly the assumption the talk is questioning.

## The arc

Five movements, in this order, for these reasons.

**Slides 1 to 3, the problem.** The origin story, the measurement gap, then the fork the whole project turns on: degree versus kind. Slide 3 uses schematic pictures and no data, so the distinction lands before any real numbers can complicate it.

**Slide 4, the roadmap.** Deliberately fourth. A roadmap given before the question exists is a table of contents nobody reads. Given after slides 1 to 3, it organizes a question the room now has.

**Slides 5 to 8, what we found.** Segments, then population shares with intervals, then the demographic breakdown, then the three-estimator comparison. Slide 6 exists to make one word land, population: a cluster analysis returns groups in a file, this returns shares of a country with uncertainty attached. Slide 7 is their standard cross-tab showing something their standard cross-tab cannot, which is the moment the method becomes theirs rather than mine. Slide 8 is the payoff for the whole design argument, framed entirely as consequences, never as estimator theory.

**Slides 9 to 13, how we got there.** The model, then the k-means comparison, then the sample design, then the uncertainty result, then the AI layer. Each answers a question the findings raised rather than preempting one.

Slide 10 answers "why not the clustering I already know," which is the second question in every head in the room and is better answered early than fielded from the floor.

Slide 11 carries the total survey error content without the framework. Four plain phrases naming which error sources this addresses: sampling error estimated from the realized design, measurement error made a parameter, item nonresponse handled by scoring partial responders, classification error introduced and then corrected. The diagram is the part that loses this audience; the four phrases are the part that persuades them.

Slide 12 volunteers the least flattering result in the project: propagating the measurement model roughly doubles the margins of error. It is on the critical path rather than in backup on purpose. An audience decides whether to trust a methods talk by whether the presenter names the weaknesses before being asked, and this one is both real and consequential for how findings should be reported.

Slide 13 places the AI use where it is defensible rather than where it is impressive. Late, after the numbers are believed, framed as constraints imposed rather than limitations discovered, and explicit that names vary between runs and never enter a calculation. Leading with AI would have made the novelty carry the talk and left the statistics looking like scaffolding. The phrase "which differences the analysis resolved" matches the language in the reports, which compute separations from pairwise tests on the replicate covariance rather than from overlapping intervals.

**Slides 14 to 15, the ask.** Portability and collaboration, then the knowledge-capture question that is the actual purpose of the briefing. Fifteen is the only slide with a decision attached and it holds three minutes of discussion, which is why nothing else is allowed to borrow from it.

## The central question being asked

The details cannot go into a policymaker product. A two-page product carries a segment name and a share; it cannot carry the design, the corrections, or the caveats. The briefing asks the room to choose among a standing methods annex, a shared archive of configurations and diagnostics, and a one-pager that travels with each product, and to answer three specific questions: what must be preserved to reproduce a published number in two years, who reviews a segment name before it reaches a briefing, and what the minimum caveat is that must appear in the product itself.

This is the reason for the briefing. Everything before it exists to earn the right to ask.

## Conventions in the deck

Every slide title is an assertion, not a topic. The line immediately under each title is the bottom line for that slide, stating what is happening and why it matters, so a reader who sees only titles and bottom lines gets the argument.

Density is controlled deliberately. Slides 1, 9 and 12 carry a single centered statement rather than columns. Slides 11 and 13 were cut to one list and two short lists respectively. The rule: if a bullet is a full sentence, it belongs in the notes.

Slides are tagged `[CORE]`, `[OPTIONAL]` or `[BACKUP]` in HTML comments. Fifteen minutes is core only. First cut is the k-means slide, and only if nobody in the room does cluster analysis. Second cut merges the model slide into the design slide.

Backup slides answer the questions a methods-literate attendee will raise: how the correction works, how the number of segments was chosen, which items are carrying the analysis, and the validation against an independent implementation. Answering from a backup slide reads as preparation; improvising reads as digression.

Speaker notes carry the intended pacing and the material cut from each slide. They project on a second screen if the `setbeameroption` line in the header is uncommented.

## What must be changed per account

The `params` block holds the study name, the concept, the sample counts, the featured demographic, and the two step-one sensitivity figures. The classification banner is a single command in the LaTeX header.

Two items are not parameterized and must be edited by hand. The four item labels in the slide 2 diagram are hardcoded and should name items from the account being briefed, since that slide is the hook and generic labels blunt it. The `step1_median` and `step1_max` params are carried from the demonstration run and must be replaced with the figures from the account's own sensitivity refit, or the slide reports someone else's number.

The deck reads one artifact, `model_fit.rds`, plus the segment label and demographic CSVs the report writes. It does not read parquet files, and it must not be pointed at a different artifact contract than the report produces, or the two will drift.

## Timing

Fifteen core slides against fifteen minutes, leaving three for the discussion on slide 15. The risk is slides 9 through 11, which hold the entire methods exposition and will want to expand. Rehearse those three against a clock and cut into backup rather than borrowing from 14 and 15, the only slides with an ask attached.
