HTMLWidgets.widget({

  name: "sfn_flow_graph",

  type: "output",

  factory: function(el, width, height) {

    return {

      renderValue: function(x) {

        var element = document.getElementById(el.id);
        var options = {
            width: parseFloat(getComputedStyle(element, null).width.replace("px", "")),
            height: 600,
            layout: x.layout,
            resizeHeight: true
        };
        var definition = x.definition;

        console.log(options);

        var graph = new sfn.StateMachineGraph(definition, x.element_id, options);

        var new_var graph.render();
      },

      resize: function(width, height) {
      }
    };
  }
});
