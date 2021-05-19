HTMLWidgets.widget({

  name: "sfn_flow_graph",

  type: "output",

  factory: function(el, width, height) {

    return {

      renderValue: function(x) {
          // Commmented out code from jupyter notebook rendering: workflow_widgets_graph.R
          // var element = document.getElementById(el.id);
          // parseFloat(getComputedStyle(element, null).width.replace("px", ""))
          var options = {
              width: width,
              height: height,
              layout: x.layout,
              resizeHeight: true
          };
          var definition = x.definition;

          var graph = new sfn.StateMachineGraph(definition, '#'+el.id, options);
          graph.render();

      },

      resize: function(width, height) {
      }
    };
  }
});
