#Start by getting the latest version/ updates:
#1) git checkout main
#2) git pull origin main
#3) install the latest civi2 package & load the packages
install.packages("remotes")
remotes::install_github("AtlanticR/civi2")
#4) make any edits that i need to _targets.R
#5) if the file hasn't actually changed run tar_invalidate("CIVI.csv")
      #the invalidate thing was needed because the CIVI.csv target had not changed, so it wasn't outdated, so it wasn't running. When you ran the invalidate command, you were telling targets that the target WAS in fact outdated and it should re-run it
      #targets doesn't track the files written by a target unless you add format = "file" to the arguments and the 'last thing' you call is the file path to the file. Which is maybe something we should do?
#6) tar_visnetwork(script = "inst/_targets.R") ...to see what is out of date
#7) tar_make(script = "inst/_targets.R") ...to produce the new data.
#8) and the new file at the end will appear:"\\wpnsbio9039519.mar.dfo-mpo.ca\sambashare\CIVI\civi2\data\CIVI.csv"

#if you tar_load("data_CIVI_Sites") first, you can work within the tar commands (on smaller thinsgs) without re-running everything.

library (DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)
### The Basics: grViz() and Graphviz DOT Language
#Describe/style nodes (shape, fillcolor, style) and  edges (color, arrowhead)
#Use label on edges for decision flows
####Move CIVI to the top of the flow ----
CIVIflow<-grViz("
  digraph flowchart {
    graph [rankdir = LR]
    node [shape = box, style = filled, fillcolor = white, fontsize = 15, margin=0.2]


#Start
A_up [shape=point, width=0, height=0, style=invis]
A [label = 'Get CIVI Sites']
{rank=same; A; A_up}
A -> A_up [constraint=false, arrowhead=none, tailport=n, weight=10]

#Indicators
subgraph cluster_process1 {
      style = rounded
      color = grey
      fontsize = 16
    DummyTitle1 [label=<
    <B>Indicators</B><BR/>
    Redistribute and Bin (1-5):<BR/>
    score of relative vulnerability>,shape=plaintext]

    B [label = 'Coastal Sensitivity Index', fillcolor= lightblue]
    C [label = 'Harbour Condition', fillcolor= lightblue]
    D [label = 'Degree of Facility Protection', fillcolor= lightblue]
    E [label = 'Relative Sea Level Change', fillcolor= tan]
    F [label = 'Ice Days Change', fillcolor= tan]
    G [label = 'Replacement Cost', fillcolor= lightpink]
    H [label = 'Harbour Utilization', fillcolor= lightpink]
    I [label = 'Proximity', fillcolor= lightpink]
}

#Components
subgraph cluster_process2 {
      style = rounded
      color = grey
      fontsize = 16
    DummyTitle2 [label=<
    <B>Components</B><BR/>
    Calculate Geometric Mean<BR/>
    of indicators>,shape=plaintext]

    J [label = 'Sensitivity', fillcolor= lightblue]
    K [label = 'Exposure', fillcolor= tan]
    L [label = 'Adaptive Capacity', fillcolor= lightpink]
}

#CIVI
    M [label = <<B>CIVI:</B>  Calculate<BR/>(geographic mean)>, fontsize = 16, margin=0.2]


# Arrows from Left
    A -> B
    A -> C
    A -> D
    A -> E
    A -> F
    A -> G
    A -> H
    A -> I

    B -> J
    C -> J
    D -> J
    E -> K
    F -> K
    G -> L
    H -> L
    I -> L

    J -> M
    K -> M
    L -> M

#    M -> N

# Right-side flow (Contextual Material  ending at N)
#Contextual Material
subgraph cluster_process3 {
      style = rounded
      color = grey
      fontsize = 16
    DummyTitle3 [label=<
    <B>Contextual Material</B>>,shape=plaintext]

  T [label='Indigenous Communities']
  U [label='Fishery Reliant Communities']
  V [label='Species Vulnerability']
}

  Z [label='Climate Risk Index\\nfor Biodiversity (CRIB)']
  Y [label='Port Level Fishing Footprint']
  X [label='Port Level Climate\\nVulnerability and Risk']
  W [label='ICERS Fisheries Landings']



{rank=same; W; X}
edge [style=invis, constraint=true]
 M -> T
 M -> U
 M -> V
 V -> X
 X -> Y
 Y -> Z
 M -> V
 V -> W

# Flow right to left toward Middle
  edge [style=solid]
  Z -> Y
  Y -> X
  X -> V
  W -> V

#CIVI Box (N at top, bigger)
  N [label = <<B>CIVI<BR/> Website</B>>,
     shape = box,
     style = filled,
     fillcolor = palegreen3,
     fontsize = 20,
     width=2,
     height=1]


#connections up to CIVI Website
{rank=same; M; N}
N -> M[style=invis, minlen=15]#push this box up to the top of the flow
A_up -> N [constraint=false]
DummyTitle1 -> N [constraint=false]
DummyTitle2 -> N [constraint=false]
M -> N [constraint=false]
DummyTitle3 -> N [constraint=false]
  }
")

CIVIflow %>%
  export_svg() %>%
  charToRaw() %>%
  rsvg_png("C:/Users/fergusonk/Documents/CIVI/CIVI2/CIVI2/R/KiyomiStuff/CIVIflowchart.png", width = 1200, height = 900)









####original----
grViz("
  digraph flowchart {
    graph [rankdir = LR]
    node [shape = box, style = filled, fillcolor = white]

#Start
A [label = 'Get CIVI Sites']

#Indicators
subgraph cluster_process1 {
      label=<
      <B>Indicators</B><BR/>Redistribute and Bin (1-5):<BR/>score of relative vulnerability
      >
      style = rounded
      color = grey
      fontsize = 14

    B [label = 'Coastal Sensitivity Index', fillcolor= lightblue]
    C [label = 'Harbour Condition', fillcolor= lightblue]
    D [label = 'Degree of Facility Protection', fillcolor= lightblue]
    E [label = 'Relative Sea Level Change', fillcolor= tan]
    F [label = 'Ice Days Change', fillcolor= tan]
    G [label = 'Replacement Cost', fillcolor= lightpink]
    H [label = 'Harbour Utilization', fillcolor= lightpink]
    I [label = 'Proximity', fillcolor= lightpink]
}

#Components
subgraph cluster_process2 {
    label=<
        <B>Components</B><BR/>Calculate Geometric Mean<BR/> of indicators
      >
      style = rounded
      color = grey
      fontsize = 14

    J [label = 'Sensitivity', fillcolor= lightblue]
    K [label = 'Exposure', fillcolor= tan]
    L [label = 'Adaptive Capacity', fillcolor= lightpink]
}

#CIVI
    M [label = 'Calculate CIVI\\n(geographic mean)']
    N [label = <<B>CIVI<BR/>Website</B>>, fillcolor= seagreen]


# Arrows from Left
    A -> B
    A -> C
    A -> D
    A -> E
    A -> F
    A -> G
    A -> H
    A -> I

    B -> J
    C -> J
    D -> J
    E -> K
    F -> K
    G -> L
    H -> L
    I -> L

    J -> M
    K -> M
    L -> M

    M -> N

# Right-side flow (Contextual Material  ending at N)
#Contextual Material
subgraph cluster_process3 {
      label=<
      <B>Contextual Material</B>
      >
      style = rounded
      color = grey
      fontsize = 14

  T [label='Indigenous Communities']
  U [label='Fishery Reliant Communities']
  V [label='Species Vulnerability']
}

  Z [label='Climate Risk Index\\nfor Biodiversity (CRIB)']
  Y [label='Port Level Fishing Footprint']
  X [label='Port Level Climate\\nVulnerability and Risk']
  W [label='ICERS Fisheries Landings']



  # Put these nodes at the same rank as C/N
#{rank=same; M; N; T}
edge [style=invis, constraint=true]
 N -> T
 N -> U
 N -> V -> X -> Y -> Z
 N -> V -> W

  # Flow right to left toward N
  edge [style=solid]
  Z -> Y
  Y -> X
  X -> V
  W -> V
  V -> N
  U -> N
  T -> N
  }
")
#### Dont alter above for now ----


