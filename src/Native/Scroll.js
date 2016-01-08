Elm.Native = Elm.Native || {};
Elm.Native.Scroll = {};
Elm.Native.Scroll.make = function make(localRuntime) {
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Scroll = localRuntime.Native.Scroll || {};
	if (localRuntime.Native.Scroll.values)
	{
		return localRuntime.Native.Scroll.values;
	}

	var NS = Elm.Native.Signal.make(localRuntime);
	var Tuple2 = Elm.Native.Utils.make(localRuntime).Tuple2;


	var offset = NS.input('Scroll.offset', Tuple2(window.pageXOffset, window.pageYOffset));


	localRuntime.addListener([offset.id], window, 'scroll', function scroll() {
		localRuntime.notify(offset.id, Tuple2(window.pageXOffset, window.pageYOffset));
	});


	return localRuntime.Native.Scroll.values = {
		offset: offset,
	};
};