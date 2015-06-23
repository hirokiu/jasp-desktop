JASPWidgets.Analysis = Backbone.Model.extend({
	defaults: {
		id: -1,
		results: {},
		status: "waiting",
		optionschanged: []
	}
});


JASPWidgets.Analyses = Backbone.Collection.extend({
	model: JASPWidgets.Analysis
});


JASPWidgets.AnalysisView = JASPWidgets.View.extend({
	views: [],

	initialize: function () {

		this.toolbar = new JASPWidgets.Toolbar({ className: "jasp-toolbar" })
		this.toolbar.setParent(this);
	},

	events: {
		'mouseenter': '_hoveringStart',
		'mouseleave': '_hoveringEnd',
	},

	_hoveringStart: function (e) {
		this.toolbar.setVisibility(true);
	},

	_hoveringEnd: function (e) {
		this.toolbar.setVisibility(false);
	},

	copyMenuClicked: function () {
		
		this.exportBegin(JASPWidgets.Export.type.CopyHTML, this.views);

		return true;
	},

	exportMenuClicked: function () {

		this.exportBegin(JASPWidgets.Export.type.SaveHTML, this.views);

		return true;
	},

	exportBegin: function (exportType, views) {

		JASPWidgets.Export.begin(this, exportType, true);
	},

	exportComplete: function (exportType, html) {
		pushHTMLToClipboard(html);
	},

	menuName: "Analysis",

	renderChildren: function ($element, result, status, metaEntry) {

		if (metaEntry.type == "title") {

			this.toolbar.titleTag = "h1";
			this.toolbar.title = result;
			this.toolbar.render();
			$element.append(this.toolbar.$el);
		}
		else if (metaEntry.type == "h1") {

			$element.append("<h2>" + result + "</h2>")
		}
		else if (metaEntry.type == "h2") {

			$element.append("<h3>" + result + "</h3>")
		}
		else {

			if (!_.has(result, "status"))
				result.status = status;

			var item = new JASPWidgets[metaEntry.type](result);

			item.on("CustomOptions:changed", function (options) {

				this.trigger("optionschanged", this.model.get("id"), options)

			}, this);

			var itemView;
			if (_.isArray(result)) {

				item.each(function (subItem) {
					if (!_.has(subItem, "status"))
						subItem.set("status", this.status);
				}, result);
				itemView = new JASPWidgets[metaEntry.type + "View"]({ className: "jasp-" + metaEntry.type, collection: item });
			}
			else
				itemView = new JASPWidgets[metaEntry.type + "View"]({ className: "jasp-" + metaEntry.type, model: item });

			this.listenTo(itemView, "toolbar:showMenu", function (obj, options) {

				this.trigger("toolbar:showMenu", obj, options);
			});

			this.views.push(itemView);

			itemView.render();
			$element.append(itemView.$el);
		}

	},

	render: function () {

		this.destroyViews();

		this.toolbar.$el.detach();

		this.$el.empty();

		var $innerElement = $('<div class="jasp-analysis-inner"></div>')

		var results = this.model.get("results");
		if (results.error) {

			var error = results.errorMessage

			error = error.replace(/\n/g, '<br>')
			error = error.replace(/  /g, '&nbsp;&nbsp;')

			$innerElement.append('<div class="error-message-box ui-state-error"><span class="ui-icon ui-icon-alert" style="float: left; margin-right: .3em;"></span>' + error + '</div>')

		}
		else if (results[".meta"]) {

			var meta = results[".meta"]

			for (var i = 0; i < meta.length; i++) {

				if (_.has(results, meta[i].name))
					this.renderChildren($innerElement, results[meta[i].name], this.model.get("status"), meta[i])
			}

		}

		this.$el.append($innerElement)

		return this;
	},
	unselect: function () {
		this.$el.removeClass("selected");
		//this.$el.addClass("unselected")
	},
	select: function () {
		this.$el.addClass("selected")
		//this.$el.removeClass("unselected");
	},
	destroyViews: function() {
		for (var i = 0; i < this.views.length; i++)
			this.views[i].close();

		this.views = [];
	},
	onClose: function () {
		this.destroyViews();
	}
});