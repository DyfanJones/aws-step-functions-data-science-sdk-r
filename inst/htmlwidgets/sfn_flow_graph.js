HTMLWidgets.widget({

  name: "sfn_flow_graph",

  type: "output",

  factory: function(el, width, height) {

    return {

      renderValue: function(x) {
          var element = document.getElementById(el.id);
          var options = {
              width: parseFloat(getComputedStyle(element, null).width.replace("px", "")),
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
