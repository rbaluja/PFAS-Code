library(DiagrammeR)

# Calculate exact proportions
reductions <- c(1538, 5320, 31552, 50740, 19412, 2477)
remaining <- c(122950, 121412, 116091, 84540, 33666, 14016)
percentages <- (reductions/remaining) * 100
normalized <- (percentages/max(percentages)) * 25  # Scale to max width of 15
normalized
grViz("
  digraph sample_reduction {
    
    # Graph settings
    graph [rankdir = TB, 
           splines = ortho,
           ranksep = 2.5,    
           nodesep = 1.5,    
           fontname = 'Helvetica'
           ]
    
    # Node settings
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
          
    
    # Nodes with numbers reduced
    a [label = <<b>All NH Births from 2010-2019</b><br/>Births: 122,950>]
    b [label = <<b>Missing Information on Birth Outcomes</b><br/>Births Remaining: 121,412<br/><font point-size='16'>(-1,538)</font>>]
    c [label = <<b>Missing Coordinates of Residence</b><br/>Births Remaining: 116,091<br/><font point-size='16'>(-5,320)</font>>]
    d [label = <<b>Do Not Receive Local Groundwater</b><br/>Births Remaining: 84,540<br/><font point-size='16'>(-31,552)</font>>]
    g [label = <<b>Missing Covariates</b><br/>Births Remaining: 11,539<br/><font point-size='16'>(-2,477)</font>>]
    f [label = <<b>Assigned Well is Further than<br/>5km from Nearest Contaminated Site </b><br/>Births Remaining: 14,016<br/><font point-size='16'>(-19,412)</font>>]
    e [label = <<b>Live Outside all PWS Boundaries</b><br/>Births Remaining: 33,666<br/><font point-size='16'>(-50,740)</font>>]
    
    # Horizontal edges on top (left to right)
    a -> b [penwidth = 0.3126301, color = '#93c5fd']
    b -> c [penwidth = 1.4601309, color = '#93c5fd']
    c -> d [penwidth = 9.0567028, color = '#93c5fd']
    
    # Vertical edge down
    d -> e [penwidth = 20.0000000, color = '#93c5fd']
    
    # Horizontal edges on bottom (right to left using dir=back)
    g -> f [penwidth = 5.8890290, color = '#93c5fd', dir=back]
    f -> e [penwidth = 19.2141218, color = '#93c5fd', dir=back]
      
    
    # Rank structures for layout
      {rank = same; a b}
      {rank = same; c d}
    {rank = same; g f e}
  }
")
