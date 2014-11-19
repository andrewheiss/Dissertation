---
title: General preliminary dissertation questions
author:
- name: Andrew Heiss
  affiliation: Duke University
  email: andrew.heiss@duke.edu
date: November 18, 2014
memo: True
to: "Judith Kelley, Tim Büthe, and Timur Kuran"
...

As I argued in my comprehensive exam paper, authoritarian regimes face institutional challenges and pressures as they work to maintain stability. This pressure leads to unconventional uses of seemingly democratic institutions—for example, dictators often (1) use legislative elections to dole out patronage instead of create representative parliaments [@Blaydes:2011; @LevitskyWay:2010] and (2) turn to foreign election monitors to bolster the external credibility of those elections, even when fully intending to commit electoral fraud [@Kelley:2012]. In other words, political institutions do weird things under authoritarianism.

Broadly speaking, I am fascinated by the interplay between authoritarian regimes, domestic civil society organizations, and transnational civil society (see Figure 1). Depending on a host of factors, domestic and international civil society organizations can either play a watchdog role against the government or become captured by the government and increase its legitimacy. Dynamics within the regime, within the INGO, and relationships between the regime, domestic civil society, and transnational civil society all can explain how INGOs operate in authoritarian contexts. 

This memo will outline three broad possible research questions that investigating these complex connections. These could potentially be individual chapters or articles, or a single topic alone could expand to the whole dissertation. I look forward to your feedback.

![Triangular relationship between an authoritarian regime and domestic and transnational civil society](figure1.pdf)


\newpage

# Question 1: How do authoritarian regimes deal with INGOs?

The first topical area I might explore deals with one direction of the link between transnational civil society and authoritarian regimes. How do authoritarian regimes use policy, legislation, and other activities to restrict INGO activity? When will they do so?

## Example question

Most authoritarian regimes have formal legislation restricting INGO activities, often through restrictions on foreign funding of domestic NGOs, onerous regulations for foreign organizations, or additional scrutiny by state security services. Often, however, a regime will disregard these restrictions or simply hold them out as threats. For example, despite months of threats of shutdown, the Carter Center just barely pulled out of Egypt as the regime finally decided to enforce new INGO restrictions. Thus, *what determines when an authoritarian regime will crack down on INGOs?*

## Possible answers

If we assume that INGOs behave as political institutions just like legislatures and judiciaries, following the idea that they provide an avenue for citizens (foreign citizens in the case of transnational civil society) to challenge the state, restrictions on INGOs should be correlated with the cohesion and stability of the regime. As authoritarian regimes face more severe political crises, the likelihood of INGO crackdowns probably increases. Conversely, it could also be that regimes will turn to INGOs more often in times of crisis as a method of guaranteeing regime legitimacy (i.e., turning to foreign election monitors prior to an election), and instead limiting INGOs during times of stability.

## Observable implications

The implications of either of these answers should be quite observable: crackdowns on INGOs should either increase or decrease during times of political crisis. 

## Data needed

This question can be answered in multiple ways, using different forms of data (much of which has yet tone collected). 

### Qualitative archival work

One approach could involve deep qualitative work on a small subset of authoritarian states. I can trace the legislative histories of each country's INGO policies, creating a narrative timeline of when each law was passed. I could then overlay that country's main political crises with those restrictions to see if there is correlation (i.e., the global economic recession in 2008 may have had an impact on Chinese laws on INGOs). These detailed comparative case studies would allow me to carefully investigate the determinants of the regime–INGO relationship. Such an approach would require legal archival research as well as interviews with policymakers (if possible) and INGO staffers who work in whichever countries I select as cases. This approach is similar to that of @Kelley:2004 and @Stroup:2012.

### Quantitative machine learning

There is an unfortunate dearth of data on INGO restrictions. @DupuyRonPrakash:2012 and @BloodgoodTremblay-BoirePrakash:2014 have created a useful NGO regulatory index based on an analysis of country-level *de jure* legal restrictions on foreign NGOs. However, their data is not publicly accessible and they will not share it until they publish a few more papers.^[Which stinks.]

To address this lack of data, I can use quantitative text analysis and machine learning algorithms to create my own dataset of either *de jure* or *de facto* restrictions. For the actual legal restrictions, I can collect anti-INGO policy legislation for a larger set of authoritarian nations and model their level of restrictions using supervised machine learning algorithms, selecting specific features and keywords in the text of the legislation to predict the severity of those restrictions over time. Alternatively, I can use other corpora of text that describe actual INGO restrictions, such as the International Center for Not-for-Profit Law's (ICNL) annual country reports.^[See [http://www.icnl.org/research/monitor/](http://www.icnl.org/research/monitor/)] I can then compare this restriction measure with other quantitative measures of regime crisis and stability to test these hypotheses.

I can measure *de facto* restrictions in a similar way, using event data algorithms on a large corpus of news articles to count the instances of authoritarian governments acting negatively toward a list of specific INGO actors. @Murdie:2012 follow a similar approach, using event data to count the instances of INGO shaming and blaming events against authoritarian regimes. Recent developments in event data processing (primarily because of the Open Event Data Alliance)^[See [http://openeventdata.org/](http://openeventdata.org/)], make this sort of research much more feasible.


\newpage

# Question 2: How do INGOs respond to regime restrictions?

While the first broad topic dealt with what *regimes* do as they interact with INGOS, the second topic addresses the other direction of the regime–INGO relationship: how do INGOs react and adapt to restrictions? Little research has shown what INGOs do to mitigate or account for the threat of authoritarian capture or co-optation. What are the mechanisms for INGO self-preservation within authoritarian target countries? How do INGOs maintain independence and objectivity while working in target countries? 

## Example question

INGOs open branches and offices in authoritarian states knowing that there is potential for the imposition of arbitrary restrictions. *What determines when INGOs accommodate obey those legal restrictions, oppose government intrusions into organizational practices, or pull out of the country entirely?* 

## Possible answer

INGOs with closer connections to foreign governments that can influence recalcitrant regimes through sanctions, aid, or other policy levers, will be more resistant to regime restrictions.

## Observable implications of that answer

INGOs devolve authority to regional and country-level offices in different degrees. Some (such as ECPAT International) may use a franchise approach, allowing organizations to operate under the INGO's name but act mostly independently, while others (such as Amnesty International) establish field offices that are closely connected to the London-based secretariat. INGOs that have more direct connections to foreign offices will likely be more aware of regime attempts to capture and restrict and in a better position to resist those attempts (calling on foreign governments for assistance), while organizations that use more independent branches will have closer relationships to the regime and may be more susceptible to capture. 

Additionally, INGOs that are based in more friendly nations may be more successful in resisting restrictions. French-based INGOs might have an advantage in Morocco, Algeria, and Tunisia because of their colonial and cultural legacies, while US-based INGOs might have an advantage in Jordan and Egypt because of those countries' strategic relationship with Israel. 

## Data needed

@Stroup:2012 provides a useful model for answering this question using case study-based research. I would need to select a handful of INGOs that work in 2–3 authoritarian nations, ensuring that the organizations vary in their reactions to restrictions and that the regimes vary in their relations to Western governments. I could then conduct a survey among those INGOs (or a broader subset of them), similar to @Buthe:2011 and Judith Kelley's ongoing human trafficking work. I would also conduct interviews (via Skype or in person) with INGO directors and branch officers.


\newpage

# Question 3: How do INGOs and NGOs work together?

The third topic addresses the role of domestic NGOs on the regime–INGO relationship. Do domestic NGOs mediate relations between transnational civil society and the state? @Buthe:2011 have shown that institutional complementarity is critical for successful global policymaking—nations with institutions that best match the structures of global rule-making bodies are best able to ensure that their policy preferences become global standards. A similar argument can be made for civil society. How does institutional complementarity facilitate links between NGOs and INGOs? What behavioral or structural changes do domestic civil society organizations make to gain the support of transnational allies? Does this dynamic work in reverse? Do INGOs change their strategies to form better links with civil society organizations in target countries?

## Example specific question

Domestic NGOs perform much of the grassroots work in advocacy and human rights, and their members often face great personal risk. These organizations are also often captured or co-opted by the regime. Can INGOs increase their influence in domestic politics by collaborating with domestic organizations? *Under what conditions will collaboration between domestic and transnational NGOs be successful in authoritarian regimes?*

## Possible answers

There are at least two possible (and testable) answers to this question. First, INGOs might use domestic NGOs as a strategic backdoor when direct restrictions on foreign activity are too onerous. Second, domestic NGOs that choose to collaborate with INGOs may do so because of institutional complementarities.

## Observable implications of those answers

If the answer to the first question is correct, there should be more collaboration between domestic and international NGOs in states where restrictions on foreign activities are more severe. Large umbrella organizations like Amnesty International and Human Rights Watch should seek out more partner organizations to, in part, "do their bidding" since they are more restricted from doing so themselves. However, this may be mitigated by restrictions on foreign funding—getting domestic NGOs to pursue transnational agendas might be more difficult if INGOs are unable to safely funnel resources to the grassroots organizations.

If institutional complementarities improve the likelihood of collaboration, there should be more collaboration between domestic and international NGOs with similar missions, organizational structures, or cultures. There should be some evidence of organizational isomorphism. Domestic human rights organizations that structure themselves to look more like Amnesty International, Human Rights Watch, ECPAT International, or other larger INGOs should (in theory) be more likely to work with them. 

## Data needed

To answer the first question, I would need data on NGO restrictions (as detailed in the first topic) and data on INGO-NGO collaboration (possibly also extracted using event data algorithms), which I could then model statistically. 

Data on institutional complementarities is more difficult to procure and, as in the other two topics, would need to be collected through qualitative case research. I would need to select a few INGOs that partner with domestic NGOs in various authoritarian regimes (using careful case selection criteria that I would devise) and interview or survey them to discover any organization-level complementarities, such as similar goals, missions, values, links between personnel, shared languages, shared culture, and so on.


\newpage

# References
\setlength{\parindent}{-0.2in}
\setlength{\leftskip}{0.2in}
\setlength{\parskip}{0pt}
\vspace*{0in}
\noindent
