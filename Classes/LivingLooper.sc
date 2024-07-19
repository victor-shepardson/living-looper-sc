// pattern: 
//	GUI elements are created only in .init methods
//	Layouts are created only in .gui methods
//  this way, the .gui method can compose other objects,
//  and the GUI can be rearranged just by replacing the layout on a view

LLTheme {
	var <color_bg, <color_fg, <color_dark, <color_text, <color_alert, <color_highlight;
	var <color_green, <color_yellow;
	var <font_label, <font_button;
	var <spacing;
	*new { |...args|
		^super.newCopyArgs(*args).init;
	}

	init {
		color_bg = color_bg ? Color(0.2,0.1,0.2);
		color_fg = color_fg ? Color(0.3,0.2,0.3);
		color_dark = color_dark ? Color(0.1,0.05,0.1);
		color_text = color_text ? Color(0.8,0.8,0.8);
		color_alert = color_alert ? Color(1,0.3,0.3);
		color_highlight = color_highlight ? Color(0.5,0.5,0.9);
		color_green = color_green ? Color(0.5,1,0.6);
		color_yellow = color_yellow ? Color(1,0.9,0.5);
		spacing = 4;
		font_label = Font.sansSerif(13);//.boldVariant;
		font_button = Font.sansSerif(13);//.boldVariant;

	}

	knob_colors {
		^ [color_fg, color_highlight, color_fg, color_highlight]
	}
}

LLLabel {

	var <item;
	var <text;
	var <theme;

	var <label;

	*new { |...args|
		^super.newCopyArgs(*args).init;
	}

	init {
		theme = theme ?? {LLTheme.new};
		label = StaticText()
			.string_(text)
			.stringColor_(theme.color_text)
			.font_(theme.font_label)
		;
		^this
	}

	gui {
		var align = (item.isKindOf(Knob) || item.isKindOf(LLPanner)).if{\center}{\left};
		^ VLayout(
			[label, align:align],
			item.class.findMethod(\gui).isNil.if{item}{item.gui}
		).spacing_(0).margins_(0);
	}
}

LLMeter : ServerMeterView {

	init { arg aserver, parent, leftUp, anumIns, anumOuts, theme;
		var innerView, viewWidth, levelIndic, palette;
		var inlabels, outlabels;

		var get_meter = {
			LevelIndicator()
			.warning_(0.9)
			.critical_(1.0)
			.drawsPeak_(true)
			.background_(theme.color_dark)
			.meterColor_(theme.color_green)
			.warningColor_(theme.color_yellow)
			.criticalColor_(theme.color_alert)
			// .numTicks_(9)
			// .numMajorTicks_(3)
		};

		/////// from ServerMeterView.init
		server = aserver;
		numIns = anumIns ?? { server.options.numInputBusChannels };
		numOuts = anumOuts ?? { server.options.numOutputBusChannels };
		view = CompositeView(parent);
		view.onClose_( { this.stop });
		//////

		theme = theme ?? {LLTheme.new}; 

		// ins
		if(numIns > 0) {
			// StaticText(view, Rect(10, 5, 100, 15))
			// .font_(Font.sansSerif(10).boldVariant)
			// .string_("Inputs");
			inmeters = Array.fill(numIns, get_meter);
			inlabels = Array.fill(numIns, { arg i;
				StaticText()
				.font_(Font.sansSerif(9).boldVariant)
				.string_((i+1).asString)
				.stringColor_(theme.color_text)
			});
		};

		// outs
		if(numOuts > 0) {
			// StaticText(view, Rect(10, 5, 100, 15))
			// .font_(Font.sansSerif(10).boldVariant)
			// .string_("Inputs");
			outmeters = Array.fill(numOuts, get_meter);
			outlabels = Array.fill(numOuts, { arg i;
				StaticText()
				.font_(Font.sansSerif(9).boldVariant)
				.string_((i+1).asString)
				.stringColor_(theme.color_text)
			});
		};

		// this does not respect the GUI pattern stated above,
		// since it it derived from SC's ServerMeterView,
		// but it's fine for now
		view.layout_(HLayout(*(
			numIns.collect{ |i| VLayout(
				inmeters[i], [inlabels[i], align:\center])}
			++ [View().minWidth_(8)]
			++ numOuts.collect{ |i| VLayout
			(outmeters[i], [outlabels[i], align:\center])}
		)).spacing_(theme.spacing).margins_(0));

		this.setSynthFunc(inmeters, outmeters);
		startResponderFunc = {this.startResponders};
		this.start;
	}
}

LLServerControl {
	var <>server;
	var <on_boot;
	var <on_device_change;
	var <theme;
	var <boot_button, <rate_box, <hblock_box, <cblock_box;
	var <inchan_box, <outchan_box, <indevice_drop, <outdevice_drop;

	var indevice_l;
	var outdevice_l;
	var rate_l;
	var hblock_l;
	var cblock_l;
	var inchan_l;
	var outchan_l;
	
	var boxwidth = 92;
	var dropwidth = 230;

	*new { |...args|
		^super.newCopyArgs(*args).init;
	}

	get_box { arg box;
		var val = box.value.asInteger;
		^ (val==0).if{nil}{val}
	}

	init {
		server = server ? Server.default;
		theme = theme ?? {LLTheme.new};
		boot_button = Button.new
		.states_([
			["start audio",theme.color_green,theme.color_fg],
			["starting...",theme.color_yellow,theme.color_bg],
			["quit audio",theme.color_alert,theme.color_bg]])
		.value_(server.serverRunning.if{2}{0})
		.toolTip_("boot SuperCollider server")
		.font_(theme.font_button)
		.action_{
			(boot_button.value==1).if{
				server.options.sampleRate = this.get_box(rate_box);
				server.options.hardwareBufferSize = this.get_box(hblock_box);
				server.options.blockSize = this.get_box(cblock_box);
				server.options.inDevice = indevice_drop.item;
				server.options.outDevice = outdevice_drop.item;
				server.options.numInputBusChannels = inchan_box.value.asInteger;
				server.options.numOutputBusChannels = outchan_box.value.asInteger;
				server.waitForBoot{
					on_boot.value;
					boot_button.value = 2;
				};
			}{
				server.quit;
				// NOTE: this is to remove the broken doOnServerBoot registered
				// by NN.ar, since we always reload models after rebooting anyway
				NN.models.do(ServerBoot.remove(_, Server.default))
			}
		};

		rate_box = NumberBox()
		.value_(48000)
		.decimals_(0)
		.background_(theme.color_bg)
		.stringColor_(theme.color_text)
		.normalColor_(theme.color_text)
		.typingColor_(theme.color_alert)
		.font_(theme.font_button)
		.maxWidth_(boxwidth)
		.toolTip_("set sampling rate (should match model)")
		// .enabled_(false)
		;

		inchan_box = NumberBox()
		.value_(server.options.numInputBusChannels)
		.decimals_(0)
		.background_(theme.color_bg)
		.stringColor_(theme.color_text)
		.normalColor_(theme.color_text)
		.typingColor_(theme.color_alert)
		.font_(theme.font_button)
		.maxWidth_(boxwidth)
		.toolTip_("set max number of input channels when opening audio device");

		outchan_box = NumberBox()
		.value_(server.options.numOutputBusChannels)
		.decimals_(0)
		.background_(theme.color_bg)
		.stringColor_(theme.color_text)
		.normalColor_(theme.color_text)
		.typingColor_(theme.color_alert)
		.font_(theme.font_button)
		.maxWidth_(boxwidth)
		.toolTip_("set max number of output channels when opening audio device");

		hblock_box = NumberBox()
		.value_(server.options.hardwareBufferSize ? 0)
		.decimals_(0)
		.background_(theme.color_bg)
		.stringColor_(theme.color_text)
		.normalColor_(theme.color_text)
		.typingColor_(theme.color_alert)
		.font_(theme.font_button)
		.maxWidth_(boxwidth)
		.toolTip_("set hardware block size (requires audio restart)");

		cblock_box = NumberBox()
		.value_(server.options.blockSize)
		.decimals_(0)
		.background_(theme.color_bg)
		.stringColor_(theme.color_text)
		.normalColor_(theme.color_text)
		.typingColor_(theme.color_alert)
		.font_(theme.font_button)
		.maxWidth_(boxwidth)
		.toolTip_("set supercollider control block size (requires audio restart)");

		indevice_drop = PopUpMenu()
		.items_(ServerOptions.inDevices)
		.background_(theme.color_bg)
		.stringColor_(theme.color_text)
		.font_(theme.font_button)
		.maxWidth_(dropwidth)
		.toolTip_("set input device (requires audio restart)")
		.action_{ on_device_change.value }
		;

		outdevice_drop = PopUpMenu()
		.items_(ServerOptions.outDevices)
		.background_(theme.color_bg)
		.stringColor_(theme.color_text)
		.font_(theme.font_button)
		.maxWidth_(dropwidth)
		.toolTip_("set output device (requires audio restart)")
		.action_{ on_device_change.value }
		;

		indevice_l = LLLabel(indevice_drop, "input device");
		outdevice_l = LLLabel(outdevice_drop, "output device");
		rate_l = LLLabel(rate_box, "sampling rate");
		hblock_l = LLLabel(hblock_box, "driver block");
		cblock_l = LLLabel(cblock_box, "control block");
		inchan_l = LLLabel(inchan_box, "in channels");
		outchan_l = LLLabel(outchan_box, "out channels");

		^this
	}

	gui {
		^VLayout(
			HLayout(
				indevice_l.gui, outdevice_l.gui
			).spacing_(theme.spacing),
			HLayout(
				rate_l.gui, hblock_l.gui, cblock_l.gui, inchan_l.gui, outchan_l.gui
			).spacing_(theme.spacing),
			boot_button,
		).spacing_(theme.spacing)
	}
}

LLMIDIMapper {
	var <theme;
	var <toggle; //Button to enable MIDI mapping
	var <clear_button; //Button to clear mappings
	var <save_button;
	var <load_button; //Buttons to open file dialog
	var <ports_list; //ListView of MIDI devices

	var <items; //IdentityDictionary of name -> LLMIDIButton or LLMIDIKnob
	var <nameToKey; //IdentityDictionary of name -> MIDI info 
	var <keyToNames; //Dictionary of MIDI info -> Set[name]
	var <>target; //current target of MIDI mapping

	*new { |...args|
		^super.newCopyArgs(*args).init;
	}

	gui {
		// var label = StaticText()
		// .string_("MIDI map:").stringColor_(theme.color_text);
		// ^HLayout([label, align:\right], toggle, save_button, load_button)
		^HLayout(toggle, clear_button, save_button, load_button)
		// ^HLayout(toggle, save_button, load_button, ports_list)
	}

	button { |name, parent, bounds|
		^LLMIDIButton(this, name.asSymbol, parent, bounds);
	}

	knob { |name, parent, bounds|
		^LLMIDIKnob(this, name.asSymbol, parent, bounds);
	}

	get_text { |key, text|
		// var miditext = "%:% ch:% port:%".format(*key);
		var miditext = "%:% ch:% % %".format(*key);
		^ text ++ " (%)".format(miditext);
	}

	set_states {
		// TODO: change knob appearance somehow?
		var buttons = items.select(_.isKindOf(LLMIDIButton));

		//removed mappings may still appear in keyToNames with empty names

		// reset all
		buttons.do{ |button| 
			var extra_text = (toggle.value==1).if{" (...)"}{""};
			button.states_(button.initial_states.collect{ |state| 
				[state[0]++extra_text] ++ state[1..]
			}, ephemeral:true)
		};

		// buttons.postln;
		// then set all in map
		nameToKey.pairsDo{ |name, key|
			var button = buttons.at(name.asSymbol);
			// [name, button, name.class].postln;
			button.notNil.if{
				button.states_(button.initial_states.collect{ |state| 
					[this.get_text(key, state[0])] ++ state[1..]
				}, ephemeral:true)
			}
		}
	}

	remove { |target| 
		var key = nameToKey[target];
		key.notNil.if{
			nameToKey.removeAt(target);
			keyToNames[key].remove(target);
		};
		"removed % <- %".format(target, key).postln;
		// nameToKey.postln;
		// keyToNames.postln;
	}

	add { |key, target|
		target.notNil.if{
			var newnames;
			var oldkey = nameToKey[target];
			// case: target -> oldkey gets replaced with target -> key
			// target must be removed from oldkey -> [names]
			oldkey.notNil.if{keyToNames[oldkey].remove(target)};
			newnames = keyToNames.atFail(key,Set.new).add(target);
			// map MIDI to name
			nameToKey.put(target, key);
			keyToNames.put(key, newnames);
			// modify button text
			this.set_states;
			"mapped % <- %".format(target, key).postln;
			// nameToKey.postln;
			// keyToNames.postln;
		}
	}

	match { |key|
		var names = keyToNames.at(key);
		^ names.collect{ |name| items.at(name.asSymbol) }.select(_.notNil);
		// var kitems = names.collect{ |name| items.at(name.asSymbol) };
		// var filtered = kitems.select(_.notNil);
		// [key, keyToNames].postln;
		// [\names, names].postln;
		// [\kitems, kitems].postln;
		// [\filtered, filtered].postln;
		// ^filtered;
	}

	get_endpoint { |src|
		^ MIDIClient.sources.detect{ arg e; e.uid == src}
		? (device:nil, name:nil);
	}

	reset {
		nameToKey = IdentityDictionary.new;
		keyToNames = Dictionary.new;
	}

	init {
		var midiDir = PathName(PathName(
			LivingLooperCore.filenameSymbol.asString
		).parentPath).parentPath +/+ "midi";

		theme = theme ?? {LLTheme.new};
		items = IdentityDictionary.new;
		this.reset;

		// global MIDI map toggle -- visually changes all MIDIButtons
		toggle = Button()
		.states_([
			["MIDI map", theme.color_highlight, theme.color_fg],
			["done", theme.color_dark, theme.color_highlight]])
		.font_(theme.font_button)
		.toolTip_("toggle MIDI mapping. when mapping, click a button in the GUI, and it will become associated with the next MIDI message to arrive.")
		.action_{
			// prevent accidentaly changing whatever was last mapped
			this.target = nil;
			// update button appearances
			this.set_states;
		};
		// [\CREATE, \TOGGLE,  toggle.identityHash, toggle.value, toggle.value.class].postln;

		clear_button = Button()
		.states_([["clear", theme.color_text, theme.color_fg]])
		.font_(theme.font_button)
		.toolTip_("clear all MIDI mappings; while mapping is in progress, clear the current item only.")
		.action_{
			(toggle.value==1).if{
				this.remove(this.target);
			}{
				this.reset;
			};
			this.set_states;
		};

		save_button = Button()
		.states_([["save map", theme.color_text, theme.color_fg]])
		.font_(theme.font_button)
		.toolTip_("save the current MIDI map as a file")
		.action_{
			Dialog.savePanel({ |path|
				nameToKey.writeArchive(path);
			}, path:midiDir)
		};

		load_button = Button()
		.states_([["load map", theme.color_text, theme.color_fg]])
		.font_(theme.font_button)
		.toolTip_("load the MIDI map from a file")
		.action_{
			Dialog.openPanel({ |path|
				"loading...".postln;
				this.reset;
				nameToKey = Object.readArchive(path);//.postln;
				nameToKey.pairsDo{ |name, key| 
					var newnames = keyToNames.atFail(key,Set.new).add(name);
					keyToNames.put(key, newnames)
					};
				this.set_states;
			}, path:midiDir)
		};
		// popup instead of load button
		// for this to work, there would have to be a 'file-first' approach
		// i.e. a "new..." menu item which would make you name the map,
		// then autosave your edits
		// this would be awkward if you want to copy one...
		// also if you want an ad-hoc map without clutter...
		// load_button = PopUpMenu()
		// .items_(midiDir.entries)
		// .background_(theme.color_bg)
		// .stringColor_(theme.color_text)
		// .font_(theme.font_button)
		// .toolTip_("load a MIDI map)
		// .action_{
		// };

		MIDIIn.connectAll;

		// ports_list = ListView()
		// .selectionMode_(\multi)
		// .items_(MIDIClient.sources.collect(_.name))
		// ;

		MIDIdef(\LLMIDIMapperOn, { |val, num, chan, src|
			var endpoint = this.get_endpoint(src);
			var key = [\note, num, chan+1, endpoint.device, endpoint.name];
			// noteOn can trigger MIDI mapping
			// noteOn sets button to state 1
			{
				// [endpoint, key, toggle.value, this.target, this.target.class].postln;	
				(toggle.value==1).if{
					// when mapping is toggled on, associate 
					// incoming message type/value/chan/src with the target
					// NoteOn maps to buttons only
					items[this.target].isKindOf(LLMIDIButton).if{
						this.add(key, this.target);
					}
				}{
					// when toggled off, a MIDI handler dispatches mapped message type/value/src to buttons
					var buttons = this.match(key);
					buttons.do{ |button|
						button.valueAction_(1);
					}
				}
			}.defer;
		}, msgType:\noteOn).permanent_(true);

		MIDIdef(\LLMIDIMapperOff, { |val, num, chan, src|
			var endpoint = this.get_endpoint(src);
			var key = [\note, num, chan+1, endpoint.device, endpoint.name];
			// noteOff sets button to state 0
			{
				var buttons = this.match(key);
				buttons.do{ |button|
					button.valueAction_(0);
				}
			}.defer;
		}, msgType:\noteOff).permanent_(true);

		MIDIdef(\LLMIDIMapperControl, { |val, num, chan, src|
			var endpoint = this.get_endpoint(src);
			var key = [\control, num, chan+1, endpoint.device, endpoint.name];
			// controlChange can trigger MIDI mapping
			{
				(this.toggle.value==1).if{
					// when mapping is toggled on, associate 
					// incoming message type/value/chan/src with the target
					this.add(key, this.target);
					// [key, this.target, keyToNames].postln;
				}{
					// when mapping is toggled off, dispatch to mapped items
					var kitems = this.match(key);
					// [key, keyToNames, kitems].postln;
					// buttons advance to next state
					kitems.select(_.isKindOf(LLMIDIButton)).do{ |item|
						item.valueAction_(item.value + 1 % item.states.size);
					};
					// knobs get the CC value scaled to 0-1
					kitems.select(_.isKindOf(LLMIDIKnob)).do{ |item|
						item.valueAction_(val/127);
					};
				}
			}.defer;
		}, msgType:\control).permanent_(true);

		MIDIdef(\LLMIDIMapperProgram, { |num, chan, src|
			var endpoint = this.get_endpoint(src);
			var key = [\program, num, chan+1, endpoint.device, endpoint.name];
			// programChange can trigger MIDI mapping
			{
				(toggle.value==1).if{
					// when mapping is toggled on, associate 
					// incoming message type/value/chan/src with the target
					// ProgramChange maps to buttons only
					items[this.target].isKindOf(LLMIDIButton).if{
						this.add(key, this.target);
					}
				}{
					// when mapping is toggled off, dispatch to mapped item
					var buttons = this.match(key);
					buttons.do{ |button|
						button.valueAction_(button.value + 1 % button.states.size);
					}
				}
			}.defer;
		}, msgType:\program).permanent_(true);
	}
}

LLMIDIButton : Button {
	var <>mapper; 
	var <>name;
	var <initial_states;
	var <wrapped_action;

	*new { |mapper, name, parent, bounds| 
		var inst = super.new(parent, bounds).mapper_(mapper).name_(name.asSymbol);
		// [parent, bounds, mapper, name].postln;
		mapper.items.add(name -> inst);
		inst.action_{};
		inst.focusGainedAction_{};
		^inst
	}

	states_{ |states, ephemeral=false| 
		ephemeral.not.if{initial_states = states};
		^super.states_(states)
	}

	valueAction_{ |v|
		// valueAction_ does not trigger MIDI mapping
		this.value_(v);
		^wrapped_action.(this);
	}

	action_{ |fn|
		wrapped_action = fn;
		^super.action_{ |...args|
			// when toggled on, buttons are disabled, 
			// and clicking a button registers it as the target of MIDI mapping
			(this.mapper.toggle.value==1).if{
				// undo the press
				this.value = this.value - 1 % this.states.size;
				this.mapper.target = this.name;
				"mapping '%'...".format(this.name).postln;
			}{
				wrapped_action.(*args)
			}
		}
	}

	focusGainedAction_{ |fn|
		^super.focusGainedAction_{ |...args|
			// when toggled on, buttons are disabled, 
			// and clicking a button registers it as the target of MIDI mapping
			(this.mapper.toggle.value==1).if{
				this.mapper.target = this.name;
				"mapping '%'...".format(this.name).postln;
			}{
				fn.(*args)
			}
		}
	}
}

LLMIDIKnob : Knob {
	var <>mapper; 
	var <>name;
	// var <initial_states;
	// var <wrapped_action;

	*new { |mapper, name, parent, bounds| 
		var inst = super.new(parent, bounds).mapper_(mapper).name_(name.asSymbol);
		// [parent, bounds, mapper, name].postln;
		mapper.items.add(name -> inst);
		// inst.action_{};
		inst.focusGainedAction_{};
		^inst
	}

	// states_{ |states, ephemeral=false| 
		// ephemeral.not.if{initial_states = states};
		// ^super.states_(states)
	// }

	// valueAction_{ |v|
	// 	// valueAction_ does not trigger MIDI mapping
	// 	this.value_(v);
	// 	^wrapped_action.(this);
	// }

	// action_{ |fn|
	// 	wrapped_action = fn;
	// 	^super.action_{ |...args|
	// 		// when mapper is toggled on, 
	// 		// clicking a knob registers it as the target of MIDI mapping
	// 		(this.mapper.toggle.value==1).if{
	// 			// TODO: reset to last value before mapper was toggled on here?
	// 			this.mapper.target = this.name;
	// 			"mapping '%'...".format(this.name).postln;
	// 		}{
	// 			wrapped_action.(*args)
	// 		}
	// 	}
	// }
	
	focusGainedAction_{ |fn|
		^super.focusGainedAction_{ |...args|
			// when toggled on, buttons are disabled, 
			// and clicking a button registers it as the target of MIDI mapping
			(this.mapper.toggle.value==1).if{
				this.mapper.target = this.name;
				"mapping '%'...".format(this.name).postln;
			}{
				fn.(*args)
			}
		}
	}
}

LLUtil {
	*unixCmd { |str, maxLineLength=1024|
		//synchronous unixCmd which streams to stdout
		var pipe, line;
		("> "++str).postln;
		pipe = Pipe.new(str, "r");
		line = pipe.getLine(maxLineLength);
		while({line.notNil}, {("\t"++line).postln; line=pipe.getLine});
		^ pipe.close
	}

	*shellQuote { |str|
		// shellQuote which works on windows
		(thisProcess.platform.name==\windows).if{
			while{str.endsWith("\\")}{str = str.drop(-1)};
			^ str.quote
		}{
			^ str.shellQuote
		}
	}
}

LivingLooperCore {
	// this is the Living Looper pseudo-UGen and other core features (no GUI) 

	*sources {
		^ thisProcess.interpreter.executeFile(PathName(
			LivingLooperCore.filenameSymbol.asString
			).parentPath +/+ "sources.scd");
	}

	*binaries {
		^ thisProcess.interpreter.executeFile(PathName(
			LivingLooperCore.filenameSymbol.asString
			).parentPath +/+ "binaries.scd");
	}

	*detectNN{
		^ \NNModel.asClass.notNil;
	}


	*installNN{ //forkIfNeeded{
		var platform = thisProcess.platform.name;
		var arch = Platform.architecture;
		var key = (platform ++ \_ ++arch).asSymbol;
		var url = LivingLooperCore.binaries[key];
		var tempDir = Platform.defaultTempDir;
		var filename = tempDir +/+ PathName(url).fileName;
		var dl_cmd, unzip_cmd, mv_cmd, cp_cmd, clean_cmd;
		dl_cmd = "curl -L % -o %";
		unzip_cmd = "tar -xvf % -C %";
		mv_cmd = (platform==\windows).if{"move % %"}{"mv % %"};
		cp_cmd = (platform==\windows).if{"copy % % /y"}{"cp % %"};
		clean_cmd = (platform==\windows).if{"del %"}{"rm %"};

		// download
		(LLUtil.unixCmd(dl_cmd.format(
			LLUtil.shellQuote(url), LLUtil.shellQuote(filename)
		))>0).if{Error("failed to download NN.ar").throw};
		// unzip
		(LLUtil.unixCmd(unzip_cmd.format(
			LLUtil.shellQuote(filename), LLUtil.shellQuote(tempDir)
		))>0).if{Error("failed to unzip NN.ar").throw};
		// move
		(LLUtil.unixCmd(mv_cmd.format(
			LLUtil.shellQuote(tempDir +/+ "nn.ar"),
			LLUtil.shellQuote(Platform.userExtensionDir +/+ "nn.ar")
		))>0).if{Error("failed to move NN.ar to Extensions").throw};
		// cleanup
		LLUtil.unixCmd(clean_cmd.format(
			LLUtil.shellQuote(filename)
		)); //ok if this fails

		(platform==\windows).if{
			(LLUtil.unixCmd(cp_cmd.format(
				LLUtil.shellQuote(Platform.userExtensionDir +/+ "nn.ar" +/+ "ignore" +/+ "*"),
				LLUtil.shellQuote(Platform.resourceDir)
			))>0).if{Error("failed to move DLLs to resource dir").throw};
		};

		(platform==\osx).if{
			LLUtil.unixCmd(
				"xattr -d -r com.apple.quarantine"
				+ (Platform.userExtensionDir +/+ "nn.ar").shellQuote)
		};
		"NN.ar has been installed".postln;
	}//}

	*load { |name, source, forceDownload=false| forkIfNeeded{
		var filename;

		var sources = LivingLooperCore.sources;
		var url = sources.at(source);
		url.notNil.if{
			// get model by name from remote source
			var modelDir = PathName(PathName(
				LivingLooperCore.filenameSymbol.asString
				).parentPath).parentPath +/+ "models";
			// var cond = Condition.new;
			// filename = modelDir.postln +/+ (name++".ts");
			filename = modelDir +/+ PathName(url).fileName;
			(forceDownload || File.exists(filename).not).if{
				var dl_cmd = "curl -L % -o %";
				// download
				// AppClock.sched(0,{
				"downloading...".postln;
				(LLUtil.unixCmd(dl_cmd.format(
					LLUtil.shellQuote(url), LLUtil.shellQuote(filename)
				))>0).if{Error("failed to download model").throw};
				"done".postln;
				// "downloading % from % to %".format(source, url, filename).postln;
				// ["======THREAD======", thisThread, thisThread.clock].postln;
				// AppClock.sched(0,{
				// // {
				// 	// ["======THREAD======", thisThread, thisThread.clock].postln;
				// 	Download(
				// 		url,
				// 		filename,
				// 		finishedFunc: {cond.test=true; cond.signal},
				// 		errorFunc: {
				// 			Error("download '%' failed".format(url)).throw},
				// 		progressFunc: { |bt, br|
				// 			"%: % of % bytes".format(source, br, bt).postln; }
				// 	);
				// 	// "downloading complete".postln;
				// });
				// cond.wait;
				// }
			}
		}{
			// assume source is a local filename
			File.exists(source).if{
				filename = source;
			}{
				Error(
					"Living Looper model '"++source++"' not recognized\n"
					++"check your file path or try one of these "
					++"available models: %".format(sources.keys)
					).throw;
			}
		};
		"loading % from %".format(name, filename).postln;
		Server.default.serverRunning.if{
			NN.load(name, filename);
			NN(name).describe;
		}{
			"WARNING: server not running".postln;
		}
	}}

	*ar { |name, input, loop=0, thru=0, auto=0, blockSize=nil|
		var out, zs; 
		var method = NN(name, \forward_with_latents);
		var out_zs = method.ar(
			input,
			blockSize,
			debug:1,
			attributes:[
				loop_index: loop, // loop index
				thru: thru, // loop mode
				auto: auto // auto trigger mode
			]);
		out = out_zs.at((0..method.numOutputs/2-1));
		zs = out_zs.at((method.numOutputs/2..method.numOutputs-1));

		^ [out, zs]
	}
}

LLPendulum {
	*draw { |view, latents, l0_sign|
		var bounds = view.bounds.width@view.bounds.height;
		var mag = latents[0] * (l0_sign?1);
		var pt;
		var seg_taper = 0.9;
		var width_taper = 0.9;
		var nspecial = 2;
		var ndrop = (latents.size - nspecial) % 3;
		var width;
		var color;
		var segment = 0@1;
		var clumps;
		// ["first latent", latents[0]].postln;
		mag = (mag.exp+1).log;
		(mag>1).if{mag = mag.sqrt};
		// pt = 0@((mag-1).clip(-0.9, 0));
		pt = 0@0;
		// mag.postln;
		width = (mag.sqrt/16).clip(0,0.2);
		// Pen.color = LLTheme.new.color_bg.alpha_(0.1);
		Pen.color = Color(0,0,0,0.1);
		// Pen.color = Color(0,0,0,0.3);
		// Pen.color = Color(0,0,0,1);
		Pen.fillRect(Rect(0,0,view.bounds.width,view.bounds.height));
		color = Color(
			// (latents[1]).sin+1/2,
			0.4,
			1,
			0.5,
			// (latents[2]).sin+1/2,
			// mag.clip(0,1)
			// 0.1
			)
			// .scaleByAlpha
			;
		Pen.addOval(Rect.fromPoints(
				pt-width +1/2*bounds, pt+width +1/2*bounds));
		Pen.color = color;
		Pen.fill;
		clumps = latents.drop(nspecial).drop(0-ndrop).clump(3);
		segment = segment 
			/ clumps.size.collect{ |i| seg_taper**(i+1) }.sum
			* mag.clip(0.1, 0.9)
			;// * mag;
		clumps.do{ |item,i|
			var new_pt;
			var new_width = width*width_taper;
			var start;
			var new_color;
			var perp;
			var rad;
			var extent = (i+1)/clumps.size;
			segment = segment.rotate(item[0]*1pi/6)*seg_taper;
			new_pt = segment + pt;
			rad = new_pt.rho;
			// new_pt = new_pt / (new_pt.rho+1);
			// new_pt = new_pt / (rad>0.5).if{rad-0.5 + 1}{1};
			// draw a quad
			perp = (new_pt - pt).rotate(0.5pi);
			perp = perp/(perp.rho+1e-7);
			start = pt+(width*perp) +1/2*bounds;
			Pen.moveTo(start);
			Pen.lineTo(new_pt+(new_width*perp) +1/2*bounds);
			Pen.lineTo(new_pt-(new_width*perp) +1/2*bounds);
			Pen.lineTo(pt-(width*perp) +1/2*bounds);
			Pen.lineTo(start);
			// // fill gradient
			new_color = Color(
				(item[1]).sin+1/2,
				// i+1/clumps.size,
				// 1-extent,
				(1-extent).pow(2.pow(0-latents[1] /2))*0.8+0.2,
				// (i/8*6).cos+1/2,
				(item[2]).sin+1/2,
				// mag.clip(0,1)
				// extent.sqrt.clip(0.2, 1)
				)
				// .scaleByAlpha
				;
			Pen.fillAxialGradient(
				pt +1/2*bounds, new_pt +1/2*bounds, color, new_color);

			// Pen.addWedge(new_pt+1/2*bounds, new_width, 0, 2pi);
			Pen.addOval(Rect.fromPoints(
				new_pt-new_width +1/2*bounds, new_pt+new_width +1/2*bounds));
			Pen.color = new_color;
			Pen.fill;
			// Pen.moveTo(pt+1/2*bounds);
			// Pen.lineTo(new_pt+1/2*bounds);
			pt = new_pt;
			width = new_width;
			color = new_color;
			// Pen.width = bounds.x/10 * size;
			// Pen.stroke;
		}
	}
}

LLBars {
	*draw { |view, latents, l0_sign| 
		var bounds = view.bounds.width@view.bounds.height;
		var num = latents.size;
		Pen.color = Color(0.2,0.1,0.2,0.25);
		Pen.fillRect(Rect(0,0,view.bounds.width,view.bounds.height));
		latents.do{ |item,i|
			var v = i+1 / (num+2);
			Pen.color = Color(
				(i*2pi/3).sin+1/2,
				(i*2pi/4).cos+1/2,
				(i*2pi/5).sin+1/2);
			Pen.moveTo((0.5@v)*bounds);
			Pen.lineTo(((item/8 + 0.5)@v)*bounds);
			Pen.width = 3;
			Pen.stroke;
		}
	}
}


LivingLooperGUI {
	// this wraps LivingLooperCore with the basic GUI 
	// (visualization, core controls, MIDI mapper) 
	// which might be integrated into other Supercollider
	// SynthDefs / interfaces
	var <name;
	var <l0_sign;
	var <mapper;
	var <theme;

	var <>id;

	var <>synth;
	var <>nLoops, <>nLatent;

	var <>debug = false;
	var <window;
	var <loopButtons;
	var <eraseButtons;
	var <autoButton;
	var <thruButton;
	var <displays;
	var <freqscopes;
	var <latents;
	var <loops_bus;

	// var <gui;

	ar { |input, blockSize=nil|
		var out, zs; 
		var mx, tr;
		# out, zs = LivingLooperCore.ar(
			name, input, 
			loop:\loop.kr(0), thru:\thru.kr(0), auto:\auto.kr(0),
			blockSize:blockSize);
		// zs are packed in audio signals;

		mx = Mix.new(zs.abs);

		// trigger on consecutive 0, integer
		tr = Delay1.ar(mx eq: 0) * (mx>0) * (mx.frac eq: 0);
		// env to fade out if there's no trigger for a while
		// var env = EnvGen.ar(Env([0,1,1,0], [0, 4096*SampleDur.ir, 0.1]), tr);
		// expand into n audio signals
		nLatent.do{ |zi|
			// var delay_s = (nLatent-1-zi) * SampleDur.ir;
			tr = Delay1.ar(tr);
			// synced individual signals (for sending to external RAVE decoders)
			// DelayN.ar(Latch.ar(sig, tr), 0.1, delay_s);
			// OSC to GUI
			SendReply.ar(tr, "/living_looper_monitor_"++this.id, zs, zi);
		}
		// zs.scope;
		^ out
	}

	*new { |...args| 
		^super.newCopyArgs(*args).init;
	}

	init {
		nLatent = NN(name, \encode).numOutputs;
		nLoops = NN(name, \forward).numOutputs;
		id = UniqueID.next;
		theme = theme ?? {LLTheme.new};

		mapper = mapper ?? {LLMIDIMapper.new};
		// [\MAPPER, mapper.identityHash, mapper.toggle.identityHash].postln;


		// loop buttons start / end recording
		loopButtons = nLoops.collect{ |i| 
			var j = i+1;
			mapper.button(\loop++j).states_([
				["record "++j.asString, theme.color_text, theme.color_fg],
				["play "++j, theme.color_alert, theme.color_bg]])
				.font_(theme.font_button)
				.toolTip_("start/end recording loop"+j);

		};
		// erase buttons 
		eraseButtons = nLoops.collect{ |i|
			var j = i+1;
			mapper.button(\erase++j).states_([
				["erase "++j.asString,theme.color_text,theme.color_dark]])
				.font_(theme.font_button)
				.toolTip_("erase loop"+j);
		};

		// TODO: possibly draw all loops into one userview so they can overlap?
		displays = nLoops.collect{ |loop_idx|
			UserView.new
			// .background_(Color.rand)
			.background_(Color.black)
			// .animate_(true)
			.resize_(5)
			.clearOnRefresh_(false)
			// .drawFunc_(LLBars.draw(_, latents[loop_idx], l0_sign))
			.drawFunc_(LLPendulum.draw(_, latents[loop_idx], l0_sign))
			;
		};

		// NOTE: for the scopes to work, the synth will need to write to loops_bus
		loops_bus = Bus.alloc(\audio, numChannels:nLoops);

		freqscopes = nLoops.collect{ |loop_idx|
			FreqScopeView()
			.active_(true)
			.freqMode_(1)
			// .dbRange
			.inBus_(loop_idx+loops_bus.index)
		};

		autoButton = mapper.button(\auto)
			.states_([
				["auto", theme.color_text, theme.color_fg],
				["auto", theme.color_alert, theme.color_bg]])
			.font_(theme.font_button)
			.toolTip_("automatically record loops in response to input");
		thruButton = mapper.button(\thru)
			.states_([
				["thru", theme.color_text, theme.color_fg], 
				["thru", theme.color_alert, theme.color_bg]])
			.font_(theme.font_button)
			.toolTip_("hear processed input while recording a loop");
		// thruButton = CheckBox().string_("thru");

		latents = (0!nLatent)!nLoops;

		OSCdef(\living_looper_monitor_++this.id, { |msg|
			// [msg[1],this.synth.nodeID].postln;
			// (msg[1]==this.synth.nodeID).if{
			var latent_idx = msg[2];
			var values = msg[3..3+nLoops-1];
			// [latent_idx, values].postln;
			values.do{ |v,loop_idx|
				latents[loop_idx][latent_idx] = v;
			};
			(latent_idx==(nLatent-1)).if{
				{displays.do(_.refresh)}.defer;
			};
			// }
		}, '/living_looper_monitor_'++this.id);

		// GUI functions

		//
		loopButtons.do{ |b,i| b.action_{ 
			// \hello.postln;
			// auto mode off
			(autoButton.value > 0).if{autoButton.valueAction_(0)};

			(b.value==1).if{
				debug.if{["LivingLooperGUI: loop", i+1].postln};
				// start recording
				synth!?(_.set(\loop, i+1));
				// any other recording stops internally -- 
				// make buttons reflect this
				loopButtons.do{ |b| b.value_(0)};
				b.value_(1);
			}{
				debug.if{["LivingLooperGUI: loop end"].postln};
				// end recording
				synth!?(_.set(\loop, 0));
			}
		}};

		eraseButtons.do{ |b,i| b.action_{ 
			// erase loop
			debug.if{["LivingLooperGUI: erase", i+1].postln};
			synth!?(_.set(\loop, -1-i));
			// any recordings stop
			loopButtons.do{ |b| b.value_(0)};
			displays[i].clearDrawing;
		}};

		autoButton.action_{
			(autoButton.value==1).if{
				debug.if{["LivingLooperGUI: auto mode", 2].postln};
				synth!?(_.set(\auto, 2))
			}{
				debug.if{["LivingLooperGUI: auto off"].postln};
				synth!?(_.set(\auto, 0))
			};
		};

		thruButton.action_{
			(thruButton.value==1).if{
				debug.if{["LivingLooperGUI: thru on"].postln};
				synth!?(_.set(\thru, 1))
			}{
				debug.if{["LivingLooperGUI: thru off"].postln};
				synth!?(_.set(\thru, 0))
			}
		};
	}

	gui {
		^ VLayout(
			HLayout(thruButton, autoButton, mapper.gui)
				.spacing_(theme.spacing),
			HLayout(*nLoops.collect{ |i| 
				[VLayout(
					VLayout(
						[displays[i], stretch:5],
						[freqscopes[i], stretch:1],
					).spacing_(0),
					eraseButtons[i],
					loopButtons[i]
				).spacing_(theme.spacing), stretch:1]
			}).spacing_(theme.spacing)
		)
	}

	destroy {
		synth !? (_.free);
		this.cleanup;
	}

	//// programmatic access to GUI actions
	erase { |idx| 
		(idx>0).if{
			this.eraseButtons[idx-1].action.defer;
		}{
			("ERROR: LivingLooperGUI: can't erase loop" + idx).postln;
		}
	}

	record { |idx| 
		(idx>0).if{
			{
				this.loopButtons[idx-1].valueAction_(1);
				this.autoButton.valueAction_(0);
			}.defer;
		}{
			("ERROR: LivingLooperGUI: can't record loop" + idx).postln;
		}
	}

	end { |idx|
		(idx>0).if{
			{
				this.loopButtons[idx-1].valueAction_(0);
				this.autoButton.valueAction_(0);
			}.defer;
		}{
			("ERROR: LivingLooperGUI: can't end loop" + idx).postln;
		}
	}

	auto {
		{this.autoButton.valueAction_(1-this.autoButton.value)}.defer;
	}

	thru {
		{this.thruButton.valueAction_(1-this.thruButton.value)}.defer;
	} 
	//////

	map { |synth|
		// synth: synth containing a LivingLooperCore UGen
		this.synth = synth;
		this.make_window.layout_(VLayout(this.gui));
		^ this
	}

	make_window {
		// GUI elements
		window = Window.new(bounds:Rect(200,250,1200,400))
			.background_(theme.color_bg)
			.onClose_{this.cleanup}
			.front;
		^ window;
	}

	cleanup {
		freqscopes !? {freqscopes.do{ |fr| fr.kill}};
	}
}

LivingLooper {
// This is the standalong Living Looper object,
// including server control, model loading, input and output routing
	var target;
	var addAction;
	var <mapper;
	var <theme;
	var <window;
	var server_control;
	var model_picker;
	var input_picker;
	var output_picker;
	var meter_view;
	var force_dl, force_dl_button;
	var input_gain_knob, dry_gain_knob, dry_panner, output_gain_knob;
	var mixers;

	var model_picker_l;
	var input_picker_l;
	var output_picker_l;
	var input_gain_l;
	var dry_gain_l;
	var dry_pan_l;
	var output_gain_l;

	var title_main, title_sub;
	var <ll;
	var ll_view;

	var in_bus_override;
	var out_bus_override;

	// var midi_state;

	var hsize = 1200;
	var vsize_init = 150;
	var vsize_default = 560;

	var window_size_unset = true;

	*new { |...args|
		// NOTE: best for apple silicon
		"OMP_NUM_THREADS".setenv("1"); 
		// "OMP_NUM_THREADS".setenv("8");

		^super.newCopyArgs(*args).init;
	}

	make_meter {
		meter_view.removeAll;
		meter_view.layout_(VLayout(LLMeter(
			server_control.server, 
			numIns:server_control.inchan_box.value.asInteger,
			numOuts:server_control.outchan_box.value.asInteger,
			).view))
		;
	}

	make_mixers {
		mixers.isNil.if{mixers = List.new};

		max(0, ll.nLoops - mixers.size).do{ |i|
			mixers.add(LLMixer(mapper, i+1))
		};

		mixers.do{ |m, i|
			var gate_action = {
				ll.synth.notNil.if{ll.synth.set(\gate, this.get_gates)}};
			m.mute_button.action_(gate_action);
			m.solo_button.action_(gate_action);
			m.panner.action_{ |knob=nil, box=nil|
				var c = this.output_picker_to_chan;
				var n = this.output_picker_to_width;
				box.isNil.if{ box = knob * n + c };
				knob.isNil.if{ knob = box - c / n };
				m.panner.knob.value_(knob);
				m.panner.box.value_(box);
				ll.notNil.if{ll.synth.set(\pan, this.get_pans)};
			};
			m.panner.knob.valueAction_(i/(mixers.size-1));

			m.visible_(i<ll.nLoops) //TODO
		};
	}

	get_gates {
		// gate = solo or not (mute or any(solo))
		// ~: 1-x
		// and: &
		// or: |
		// any: sum.sign
		// solo | (1 - (mute | solo.sum.sign))
		var mutes = mixers.collect{ |m| m.mute_button.value};
		var solos = mixers.collect{ |m| m.solo_button.value};
		^ solos | (1 - (mutes | solos.sum.sign))
	}

	get_pans {
		^ mixers.collect{ |m| m.panner.value} 
	}

	make_synth {
		var vspace = window.bounds.height + 50;

		server_control.server.serverRunning.if{

			this.set_io_options;

			this.make_meter;

			ll_view.notNil.if{ll_view.removeAll};
			ll = LivingLooperGUI(\standalone, nil, mapper);

			this.make_mixers;

			// copy previous MIDI mapper state over
			// midi_state.notNil.if{ 
				// ll.mapper.nameToKey.putAll(midi_state); 
				// ll.mapper.set_states;
			// };
			// create Synth on the server
			ll.synth = SynthDef(\ll++ll.id, {
				var nchan = server_control.server.options.numOutputBusChannels;
				var in = 
					In.ar(\inbus.kr(this.get_input_bus))
					* \input_gain.kr(input_gain_knob.value);
				var loops = 
					ll.ar(in, blockSize:nil)
					* \gate.kr(this.get_gates);

				var mix = Mix(PanAz.ar(
					nchan, 
					loops, 
					\pan.kr(this.get_pans) - 1 * (2/nchan),
					orientation:0
					));

				mix = \dry_gain.kr(dry_gain_knob.value) 
					* PanAz.ar(
						nchan, 
						in, 
						\dry_pan.kr(dry_panner.value) - 1 * (2/nchan),
						orientation:0
					) + mix;

				mix = Limiter.ar(
					mix * 2 * \output_gain.kr(output_gain_knob.value));

				Out.ar(\outbus.kr(this.get_output_bus), mix);

				// var stereo = 
				// 	\dry_gain.kr(dry_gain_knob.value)/2.sqrt * in 
				// 	+ Splay.ar(loops);
				// stereo = Limiter.ar(
				// 	stereo * 2 * \output_gain.kr(output_gain_knob.value));
				// Out.ar(\outbus.kr(this.get_output_bus), stereo);

				Out.ar(ll.loops_bus, loops); //this is used by the visualization
			}).play(target?Server.default, addAction:addAction?\addToHead);

			// add GUI to window
			// window.layout.add(ll.gui, stretch:1);
			// window.asView.removeAll;
			window.layout_(this.gui);
			// increase window size
			window_size_unset.if{
				window.setInnerExtent(
					window.bounds.width, 
					max(0, vsize_default - vspace) 
					* window.bounds.width / hsize
					+ vspace
				);
				window_size_unset = false;
			};
			// allow quitting the model picker without anything happening
			model_picker.allowsReselection_(false);
		}
	}

	stop_synth {
		ll.notNil.if{
			// midi_state = ll.mapper.nameToKey;
			ll.destroy;
		};
		// allow using the model picker to start a LivingLooperGUI instance
		// without rebooting the server
		model_picker.allowsReselection_(true);
	}

	install {
		LivingLooperCore.detectNN.not.if{
			// dialog to confirm installing NN
			var cond = Condition.new;
			"WARNING: NN.ar not found".postln;
			Window("install NN.ar")
			.setInnerExtent(500,100)
			.front
			.background_(theme.color_bg)
			.layout_(VLayout(
				StaticText()
				.string_("Living Looper requires the NN.ar SuperCollider extension.")
				.stringColor_(theme.color_highlight),
				HLayout(
					Button()
					.states_([["Automatically install NN.ar", theme.color_text, theme.color_fg]])
					.toolTip_("install NN.ar and restart SuperCollider")
					.action_{
						"attempting to install...".postln;
						LivingLooperCore.installNN;
						cond.test=true; cond.signal
					},
					Button()
					.states_([["No thanks, I'll install it myself", theme.color_text, theme.color_fg]])
					.toolTip_("open the download page for NN.ar")
					.action_{
						"please install NN.ar and restart SuperCollider".postln;
						"https://github.com/elgiano/nn.ar/releases".openOS;
						cond.test=true; cond.signal
					},
			)));
			cond.wait;
			cond.test=false;
			Window("install NN.ar")
			.setInnerExtent(500,100)
			.front
			.background_(theme.color_bg)
			.layout_(VLayout(
				StaticText()
				.string_("that appears to have worked. The sclang interpreter will now restart, and then you can run LivingLooper.new again")
				.stringColor_(theme.color_highlight),
				HLayout(
					Button()
					.states_([["ok", theme.color_text, theme.color_fg]])
					.toolTip_("restart sclang interpreter")
					.action_{
						cond.test=true; cond.signal
					}
			)));
			cond.wait;
			"restarting interpreter...".postln;
			thisProcess.recompile;
			cond.test=false; cond.wait;
		};
	}

	set_io_options {

		var n_in = server_control.server.options.numInputBusChannels;
		var n_out = server_control.server.options.numOutputBusChannels;
		var inval = input_picker.value ? 0; //first channel
		var outval = output_picker.value ? n_out; //first stereo

		input_picker.items_(n_in.collect{ |i| i+1 });
		inval.notNil.if{(inval < input_picker.items.size).if{
			input_picker.value_(inval)}};

		output_picker.items_(
			n_out.collect{ |i| "%".format(i+1) } //mono options
			++
			(n_out-1).collect{ |i| "%-%".format(i+1, i+2) } //stereo options
			++
			["multi"]
			);
		outval.notNil.if{(outval < output_picker.items.size).if{
			output_picker.value_(outval)}};
	}

	get_input_bus {
		// return input bus index
		^ in_bus_override ? (input_picker.value 
			+ server_control.server.options.numOutputBusChannels);
	}
	set_input_bus {
		ll.notNil.if{ll.synth.set(\inbus, this.get_input_bus)}
	}

	output_picker_to_chan {
		var i = output_picker.value;
		var n = output_picker.items.size;
		var c = (n/2).asInteger;
		// first N are mono,
		// next N-1 are stereo,
		// last is multichannel
		^(i==(n-1)).if{0}{i%c} + 1
	}
	output_picker_to_width {
		var i = output_picker.value;
		var n = output_picker.items.size;
		var c = n/2;
		// [i,n,c].postln;
		^(i==(n-1)).if{c-1}{(i/c).asInteger}
	}
	get_output_bus {
		^ out_bus_override ? 0
	}
	set_output_bus {
		ll.notNil.if{ll.synth.set(\outbus, this.get_output_bus)}
	}

	init {
		// put these startup steps in a Routine, 
		// so that when it is played on the AppClock,
		// GUI calls and Condition.wait are both available
		var r = Routine{
			this.install;
			"-----LOAD-----".postln;
			LivingLooperCore.load(
				\standalone, model_picker.item, 
				forceDownload: force_dl);
			force_dl = false;
			"-----STOP-----".postln;
			this.stop_synth;
			"-----MAKE-----".postln;
			this.make_synth;
		};
		force_dl = false;

		mapper = mapper ?? {LLMIDIMapper.new};
		// [\MAPPER, mapper.identityHash, mapper.toggle.identityHash].postln;
		theme = theme ?? {LLTheme.new};

		server_control = LLServerControl.new(
			Server.default, 
			{r.reset; r.play(AppClock)},
			{/* on device change */}
		);

		model_picker = PopUpMenu()
		.allowsReselection_(true)
		.minWidth_(270)
		.items_(LivingLooperCore.sources.keys.asList++["..."])
		.background_(theme.color_highlight)
		.stringColor_(theme.color_dark)
		.font_(theme.font_button)
		.toolTip_("choose a Living Looper model")
		.action_{
			(model_picker.item=="...").if{
				Dialog.openPanel({ |path|
					this.load(path)
				})
				// Dialog.openPanel(this.load)
			}{
				r.reset; r.play(AppClock);
			}
		};

		force_dl_button = Button()
		.states_([["dl", theme.color_alert, theme.color_fg]])
		.maxWidth_(25)
		.font_(theme.font_button)
		.toolTip_("force download of current model (to get updates)")
		.action_{
			force_dl = true;
			r.reset; r.play(AppClock);
		};

		input_picker = PopUpMenu()
		// .minWidth_(300)
		.background_(theme.color_bg)
		.stringColor_(theme.color_text)
		.font_(theme.font_button)
		.toolTip_("choose a mono input channel")
		.action_{this.set_input_bus}
		;
		output_picker = PopUpMenu()
		// .minWidth_(300)
		.background_(theme.color_bg)
		.stringColor_(theme.color_text)
		.font_(theme.font_button)
		.toolTip_("choose a range of output channels")
		.action_{
			this.set_output_bus;
			// update panner display
			mixers.notNil.if{mixers.do{ |m| 
				m.panner.knob.action.()
			}};
		}
		;

		this.set_io_options;

		meter_view = View().maxHeight_(80);
		this.make_meter;

		title_main = StaticText()
			.string_("Living Looper")
			.stringColor_(theme.color_highlight)
			.font_(Font("Helvetica", 60));
		title_sub =	StaticText()
			.string_("v1.2.0b")
			.stringColor_(theme.color_highlight)
			.font_(Font("Helvetica", 32));

		input_gain_knob = mapper.knob(\input_gain)
		.color_(theme.knob_colors)
		.mode_(\vert)
		.action_{
			ll.notNil.if{ll.synth.set(\input_gain, input_gain_knob.value)}
		}
		.toolTip_("input gain")
		.value_(0.5);

		dry_gain_knob = mapper.knob(\dry_gain)
		.color_(theme.knob_colors)
		.mode_(\vert)
		.action_{
			ll.notNil.if{ll.synth.set(\dry_gain, dry_gain_knob.value)}
		}
		.toolTip_("dry gain relative to input")
		.value_(0);

		dry_panner = LLPanner(mapper, \_dry)
		.action_{ |knob=nil, box=nil|
			var c = this.output_picker_to_chan;
			var n = this.output_picker_to_width;
			box.isNil.if{ box = knob * n + c };
			knob.isNil.if{ knob = box - c / n };
			dry_panner.knob.value_(knob);
			dry_panner.box.value_(box);
			ll.notNil.if{ll.synth.set(\dry_pan, dry_panner.value)};
		};

		output_gain_knob = mapper.knob(\output_gain)
		.color_(theme.knob_colors)
		.mode_(\vert)
		.action_{
			ll.notNil.if{ll.synth.set(\output_gain, output_gain_knob.value)}
		}
		.toolTip_("master output gain")
		.value_(0.5);

		model_picker_l = LLLabel(model_picker, "Model");
		input_picker_l = LLLabel(input_picker, "Input");
		output_picker_l = LLLabel(output_picker, "Output");

		input_gain_l = LLLabel(input_gain_knob, "Input");
		dry_gain_l = LLLabel(dry_gain_knob, "Dry");
		dry_pan_l = LLLabel(dry_panner, "Dry Pan");
		output_gain_l = LLLabel(output_gain_knob, "Output");

		window.isNil.if{ window = 
			Window.new("Living Looper", bounds:Rect(200, 500, hsize, vsize_init))
		};
		window
		.background_(theme.color_bg)
		.layout_(this.gui)
		.onClose_{
			ll.notNil.if{ll.cleanup};
		}
		.front;

		^this
	}

	gui {
		^ VLayout(
			[HLayout(
				[VLayout(
					title_main, 
					title_sub
					).spacing_(0), stretch:2, align:\center],
				[VLayout(
					// model_picker_l.gui,
					HLayout(model_picker_l.gui, force_dl_button),
					HLayout(
						input_picker_l.gui,
						meter_view,
						output_picker_l.gui
					)
				), stretch:1, align:\center],
				[server_control.gui, stretch:0],
			), stretch:0],
			[HLayout(
				input_gain_l.gui,
				output_gain_l.gui,
				dry_gain_l.gui,
				dry_pan_l.gui,
			), stretch:0],
			ll!?{ll_view = View().layout_(ll.gui); [ll_view, stretch:1]},
			mixers!?{[HLayout(*mixers.collect{ |g| g.gui}), stretch:0]}
			// LLGUI
			// mixers
		)
	}

	// programmatic control from sclang

	inputGain { |gain| input_gain_knob.valueAction_(gain) }
	outputGain { |gain| output_gain_knob.valueAction_(gain) }
	dryGain { |gain| dry_gain_knob.valueAction_(gain) }
	dryPan { |pan| dry_panner.knob.valueAction_(pan) }
	dryChan { |ch| dry_panner.box.valueAction_(ch) }
	load { |path| 
		// check if path already in picker
		var idx = model_picker.items.indexOfEqual(path);
		idx.isNil.if{
			// if file doesn't exist, abort
			File.exists(path).not.if{ 
				"file does not exist".postln;
				^nil 
			};
			// otherwise add the path to the menu
			model_picker.items = [path] ++ model_picker.items;
			idx = 0;
		};
		model_picker.valueAction_(idx);
	}

	// pass-through to LivingLooperGUI
	erase { |idx| ll.erase(idx) }
	record { |idx| ll.record(idx) }
	end { |idx| ll.end(idx)	}
	auto { ll.auto }
	thru { ll.thru } 

	// mute { |idx| } //TODO default toggle, optional value
	// solo { |idx| } //TODO default toggle, optional value
	loopPan { |idx, pan| mixers[idx-1].panner.knob.valueAction_(pan)} 
	loopChan { |idx, ch| mixers[idx-1].panner.box.valueAction_(ch)} 

	setInBus { |idx|
		input_picker.enabled_(idx.isNil);
		in_bus_override = idx;
		this.set_input_bus;
	}
	setOutBus { |idx|
		// set to multichannel mode
		output_picker.valueAction_(output_picker.items.size-1);
		output_picker.enabled_(idx.isNil);
		out_bus_override = idx;
		this.set_output_bus;
	}

	getLoopBus { ^ ll.loops_bus }

	asTarget { ^ ll.synth.asTarget }
}

LLPanner {
	var mapper;
	var name;
	var <theme;
	var <knob;
	var <box;

	var <>action;

	*new { |...args|
		^super.newCopyArgs(*args).init;
	}

	init {
		theme = theme ?? {LLTheme.new};

		knob = mapper.knob(\pan++name)
		.color_(theme.knob_colors)
		.mode_(\vert)
		.toolTip_("pan (in stereo or multichannel mode)")
		.value_(0.5)
		.action_{
			action.(knob:knob.value);
		};

		box = NumberBox()
		.background_(theme.color_bg)
		.stringColor_(theme.color_text)
		.normalColor_(theme.color_text)
		.typingColor_(theme.color_alert)
		.value_(1)
		.maxWidth_(36)
		.action_{
			action.(box:box.value);
		};
	}

	gui {
		^ HLayout(
			[knob, align:\right], [box, align:\left]
		)//.spacing_(0)
	}

	value {
		^ box.value
	}
}

LLMixer {
	var mapper;
	var name;
	var <theme;

	var <>visible; // TODO

	var <panner;
	var <mute_button;
	var <solo_button;

	*new { |...args|
		^super.newCopyArgs(*args).init;
	}

	init {
		var mw = 64;
		theme = theme ?? {LLTheme.new};

		panner = LLPanner(mapper, name);
		mute_button = mapper.button(\mute++name)
		.maxWidth_(mw)
		.states_([
			["mute",theme.color_text,theme.color_fg],
			["mute",theme.color_alert,theme.color_bg]])
		;
		solo_button = mapper.button(\solo++name)
		.maxWidth_(mw)
		.states_([
			["solo",theme.color_text,theme.color_fg],
			["solo",theme.color_yellow,theme.color_bg]])
		;
	}

	gui {
		^ HLayout(
			VLayout(mute_button, solo_button), panner.gui
		)
	}
}