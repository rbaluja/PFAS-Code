library(DiagrammeR)

# Calculate exact proportions for edge weights
reductions <- c(1538, 5320, 31552, 50740, 19412, 2477)
remaining <- c(122950, 121412, 116091, 84540, 33666, 14016)
percentages <- (reductions/remaining) * 100
normalized <- (percentages/max(percentages)) * 25  # Scale for visibility

# Create flow diagram
graph = grViz("
  digraph sample_reduction {
    
    graph [rankdir = TB, 
           splines = ortho,
           ranksep = 0.75,    
           nodesep = 0.4,    
           fontname = 'Helvetica']
    
    node [shape = 'rectangle',
          style = 'rounded,filled',
          fillcolor = '#f0f9ff',
          color = '#60a5fa',
          fontname = 'Helvetica',
          fontsize = 18,
          fontcolor = '#1e293b',
          width = 5.5,
          height = 1.2,
          penwidth = 2,
          margin = 0.3]
    
    a [label = <<b>All NH Births from 2010-2019</b><br/>Births: 122,950>]
    b [label = <<b>Missing Information on Birth Outcomes</b><br/>Births Remaining: 121,412<br/><font point-size='16'>(-1,538)</font>>]
    c [label = <<b>Missing Coordinates of Residence</b><br/>Births Remaining: 116,091<br/><font point-size='16'>(-5,320)</font>>]
    d [label = <<b>Do Not Receive Local Groundwater</b><br/>Births Remaining: 84,540<br/><font point-size='16'>(-31,552)</font>>]
    e [label = <<b>Live Outside all PWS Boundaries</b><br/>Births Remaining: 33,666<br/><font point-size='16'>(-50,740)</font>>]
    f [label = <<b>Assigned Well is Further than<br/>5km from Nearest Contaminated Site </b><br/>Births Remaining: 14,016<br/><font point-size='16'>(-19,412)</font>>]
    g [label = <<b>Missing Covariates</b><br/>Births Remaining: 11,539<br/><font point-size='16'>(-2,477)</font>>]
    
    # Edges with calculated weights
    edge [minlen=1]
    a -> b [penwidth = 0.3126301, color = '#93c5fd']
    b -> c [penwidth = 1.4601309, color = '#93c5fd']
    c -> d [penwidth = 9.0567028, color = '#93c5fd']
    d -> e [penwidth = 20.0000000, color = '#93c5fd']
    e -> f [penwidth = 19.2141218, color = '#93c5fd']
    f -> g [penwidth = 5.8890290, color = '#93c5fd']
  }
")

graph_svg <- DiagrammeRsvg::export_svg(graph)
rsvg::rsvg_png(charToRaw(graph_svg), "Figures Revision/sample_fig.png", width = 600, height = 1500)
