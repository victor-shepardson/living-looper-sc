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

	label { |item, text, view=false|
		var align = (item.class==Knob).if{\center}{\left};
		var layout = VLayout(
			[
				StaticText().string_(text)
				.stringColor_(color_text)
				.font_(font_label)
				, align:align],
			item
		).spacing_(0).margins_(0);
		^ view.if{
			View().layout_(layout)
		}{
			layout
		}
	}

	knob_colors {
		^ [color_fg, color_highlight, color_fg, color_highlight]
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

		theme = theme ? LLTheme.new; 

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
	var <theme;
	var <boot_button, <rate_box, <hblock_box, <cblock_box;
	var <inchan_box, <outchan_box, <indevice_drop, <outdevice_drop;

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
		theme = theme ? LLTheme.new;
		boot_button = Button.new
		.states_([
			["start audio",theme.color_green,theme.color_fg],
			["starting...",theme.color_yellow,theme.color_bg],
			["quit audio",theme.color_alert,theme.color_bg]])
		.value_(server.serverRunning.asInteger)
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
		.value_(8)
		.decimals_(0)
		.background_(theme.color_bg)
		.stringColor_(theme.color_text)
		.normalColor_(theme.color_text)
		.typingColor_(theme.color_alert)
		.font_(theme.font_button)
		.maxWidth_(boxwidth)
		.toolTip_("set max number of input channels when opening audio device");

		outchan_box = NumberBox()
		.value_(8)
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
		;

		outdevice_drop = PopUpMenu()
		.items_(ServerOptions.outDevices)
		.background_(theme.color_bg)
		.stringColor_(theme.color_text)
		.font_(theme.font_button)
		.maxWidth_(dropwidth)
		.toolTip_("set output device (requires audio restart)")
		;

		^this
	}

	gui {
		^VLayout(
			HLayout(
				theme.label(indevice_drop, "input device"),
				theme.label(outdevice_drop, "output device")
			).spacing_(theme.spacing),
			// HLayout(
			// 	theme.label(inchan_box, "input channels"),
			// 	theme.label(outchan_box, "output channels")
			// ),
			HLayout(
				theme.label(rate_box, "sampling rate"),
				theme.label(hblock_box, "driver block"),
				theme.label(cblock_box, "control block"),
				theme.label(inchan_box, "in channels"),
				theme.label(outchan_box, "out channels")
			).spacing_(theme.spacing),
			boot_button,
		).spacing_(theme.spacing)
	}
}

LLMIDIMapper {
	var <theme;
	var <toggle; //Button to enable MIDI mapping
	var <save_button;
	var <load_button; //Buttons to open file dialog
	var <buttons; //Dictionary of name -> Button
	var <map; //Dictionary of MIDI info -> name
	var <>target; //current target of MIDI mapping

	*new { |...args|
		^super.newCopyArgs(*args).init;
	}

	gui {
		// var label = StaticText()
		// .string_("MIDI map:").stringColor_(theme.color_text);
		// ^HLayout([label, align:\right], toggle, save_button, load_button)
		^HLayout(toggle, save_button, load_button)
	}

	button { |name, parent, bounds|
		^LLMIDIButton(this, name, parent, bounds);
	}

	get_text { |key, text|
		var miditext = "%:% ch:% port:%".format(*key);
		^ text ++ " (%)".format(miditext);
	}

	set_states {
		// reset all
		buttons.do{ |button| 
			var extra_text = (toggle.value==1).if{" (...)"}{""};
			button.states_(button.initial_states.collect{ |state| 
				[state[0]++extra_text] ++ state[1..]
			}, ephemeral:true)
		};

		// then set all in map
		map.pairsDo{ |key, name|
			var button = buttons.at(name);
			button.states_(button.initial_states.collect{ |state| 
				[this.get_text(key, state[0])] ++ state[1..]
			}, ephemeral:true)
		}
	}

	add { |key, target|
		var button, miditext;
		// map MIDI to name
		map.add(key -> target);
		map.postln;
		// modify button text
		this.set_states;
	}

	match { |key|
		var name = map.at(key);
		^buttons.at(name);
	}

	init {
		theme = theme ? LLTheme.new;
		buttons = Dictionary.new;
		map = Dictionary.new;

		// global MIDI map toggle -- visually changes all MIDIButtons
		toggle = Button()
		.states_([
			["MIDI map", theme.color_highlight, theme.color_fg],
			["done", theme.color_dark, theme.color_highlight]])
		.font_(theme.font_button)
		.toolTip_("toggle MIDI mapping. when mapping, click a button in the GUI, and it will become associated with the next MIDI message to arrive.")
		.action_{
			// update button appearances
			this.set_states;
		};

		save_button = Button()
		.states_([["save map", theme.color_text, theme.color_fg]])
		.font_(theme.font_button)
		.toolTip_("save the current MIDI map as a file")
		.action_{
			Dialog.savePanel({ |path|
				map.writeArchive(path);
			})
		};

		load_button = Button()
		.states_([["load map", theme.color_text, theme.color_fg]])
		.font_(theme.font_button)
		.toolTip_("load the MIDI map from a file")
		.action_{
			Dialog.openPanel({ |path|
				"loading...".postln;
				map = Object.readArchive(path).postln;
				this.set_states;
			})
		};

		MIDIdef(\LLMIDIMapperOn, { |val, num, chan, src|
			var key = [\note, num, chan, src];
			// noteOn can trigger MIDI mapping
			// noteOn sets button to state 1
			{
				(toggle.value==1).if{
					// when mapping is toggled on, a MIDI handler associates 
					// incoming messages type/value/chan/src with the target button
					this.add(key, this.target);
				}{
					// when toggled off, a MIDI handler dispatches mapped message type/value/src to buttons
					var button = this.match(key);
					button.notNil.if{
						button.valueAction_(1);
					}
				}
			}.defer;
		}, msgType:\noteOn).permanent_(true);

		MIDIdef(\LLMIDIMapperOff, { |val, num, chan, src|
			var key = [\note, num, chan, src];
			// noteOff sets button to state 0
			{
				var button = this.match(key);
				button.notNil.if{
					button.valueAction_(0);
				}
			}.defer;
		}, msgType:\noteOff).permanent_(true);

		MIDIdef(\LLMIDIMapperControl, { |val, num, chan, src|
			var key = [\control, num, chan, src];
			// controlChange can trigger MIDI mapping
			// controlChange toggles button 
			{
				(this.toggle.value==1).if{
					// when mapping is toggled on, a MIDI handler associates 
					// incoming messages type/value/chan/src with the target button
					this.add(key, this.target);
				}{
					// when toggled off, a MIDI handler dispatches mapped message type/value/src to buttons
					var button = this.match(key);
					button.notNil.if{
						button.valueAction_(button.value + 1 % button.states.size);
					}
				}
			}.defer;
		}, msgType:\control).permanent_(true);

		MIDIdef(\LLMIDIMapperProgram, { |num, chan, src|
			var key = [\program, num, chan, src];
			// controlChange can trigger MIDI mapping
			// controlChange toggles button 
			{
				(toggle.value==1).if{
					// when mapping is toggled on, a MIDI handler associates 
					// incoming messages type/value/chan/src with the target button
					this.add(key, this.target);
				}{
					// when toggled off, a MIDI handler dispatches mapped message type/value/src to buttons
					var button = this.match(key);
					button.notNil.if{
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
	var <>initial_states;

	*new { |mapper, name, parent, bounds| 
		var inst = super.new(parent, bounds).mapper_(mapper).name_(name);
		// [parent, bounds, mapper, name].postln;
		mapper.buttons.add(name -> inst);
		^inst
	}

	states_{ |states, ephemeral=false| 
		ephemeral.not.if{this.initial_states = states};
		^super.states_(states)
	}

	action_{ |fn|
		^super.action_{ |...args|
			// when toggled on, buttons are disabled, 
			// and clicking a button registers it as the target of MIDI mapping
			(this.mapper.toggle.value==1).if{
				// undo the press
				this.value = this.value - 1 % this.states.size;
				this.mapper.target = this.name;
				"mapping '%'...".format(this.name).postln;
			}{
				fn.(*args)
			}
		}
	}

}

LivingLooper {

	*sources {
		^ thisProcess.interpreter.executeFile(PathName(
			LivingLooper.filenameSymbol.asString
			).parentPath +/+ "sources.scd");
	}

	*binaries {
		^ thisProcess.interpreter.executeFile(PathName(
			LivingLooper.filenameSymbol.asString
			).parentPath +/+ "binaries.scd");
	}

	*detectNN{
		^ \NNModel.asClass.notNil;
	}

	*installNN{ //forkIfNeeded{
		var unixCmdPostStdOut = { arg str, maxLineLength=1024;
			var pipe, line;
			("> "++str).postln;
			pipe = Pipe.new(str, "r");
			line = pipe.getLine(maxLineLength);
			while({line.notNil}, {("\t"++line).postln; line=pipe.getLine});
			pipe.close;
		};
		var platform = thisProcess.platform.name;
		var shellQuote = { arg str;
			(platform==\windows).if{
				while{str.endsWith("\\")}{str = str.drop(-1)};
				str.quote
			}{
				str.shellQuote
			}
		};
		var arch = Platform.architecture;
		var key = (platform ++ \_ ++arch).asSymbol;
		var url = LivingLooper.binaries[key];
		var tempDir = Platform.defaultTempDir;
		var filename = tempDir +/+ PathName(url).fileName;
		var dl_cmd, unzip_cmd, mv_cmd, cp_cmd, clean_cmd;
		dl_cmd = "curl -L % -o %";
		unzip_cmd = "tar -xvf % -C %";
		mv_cmd = (platform==\windows).if{"move % %"}{"mv % %"};
		cp_cmd = (platform==\windows).if{"copy % % /y"}{"cp % %"};
		clean_cmd = (platform==\windows).if{"del %"}{"rm %"};

		// download
		(unixCmdPostStdOut.(dl_cmd.format(
			shellQuote.(url), shellQuote.(filename)
		))>0).if{Error("failed to download NN.ar").throw};
		// unzip
		(unixCmdPostStdOut.(unzip_cmd.format(
			shellQuote.(filename), shellQuote.(tempDir)
		))>0).if{Error("failed to unzip NN.ar").throw};
		// move
		(unixCmdPostStdOut.(mv_cmd.format(
			shellQuote.(tempDir +/+ "nn.ar"),
			shellQuote.(Platform.userExtensionDir +/+ "nn.ar")
		))>0).if{Error("failed to move NN.ar to Extensions").throw};
		// cleanup
		unixCmdPostStdOut.(clean_cmd.format(
			shellQuote.(filename)
		)); //ok if this fails

		(platform==\windows).if{
			(unixCmdPostStdOut.(cp_cmd.format(
				shellQuote.(Platform.userExtensionDir +/+ "nn.ar" +/+ "ignore" +/+ "*"),
				shellQuote.(Platform.resourceDir)
			))>0).if{Error("failed to move DLLs to resource dir").throw};
		};

		(platform==\osx).if{
			unixCmdPostStdOut.(
				"xattr -d -r com.apple.quarantine"
				+ (Platform.userExtensionDir +/+ "nn.ar").shellQuote)
		};
		"NN.ar has been installed".postln;
	}//}

	*load { |name, source, forceDownload=false| forkIfNeeded{
		var filename;

		var sources = LivingLooper.sources;
		var url = sources.at(source);
		url.notNil.if{
			// get model by name from remote source
			var modelDir = PathName(PathName(
				LivingLooper.filenameSymbol.asString
				).parentPath).parentPath +/+ "models";
			var cond = Condition.new;
			// filename = modelDir.postln +/+ (name++".ts");
			filename = modelDir +/+ PathName(url).fileName;
			(forceDownload || File.exists(filename).not).if{
				"downloading % from % to %".format(source, url, filename).postln;
				// ["======THREAD======", thisThread, thisThread.clock].postln;
				AppClock.sched(0,{
				// {
					// ["======THREAD======", thisThread, thisThread.clock].postln;
					Download(
						url,
						filename,
						finishedFunc: {cond.test=true; cond.signal},
						errorFunc: {
							Error("download '%' failed".format(url)).throw},
						progressFunc: { |bt, br|
							"%: % of % bytes".format(source, br, bt).postln; }
					);
					// "downloading complete".postln;
				});
				cond.wait;
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

	*ar { |name, input, loop=0, thru=0, auto=0, blockSize=0|
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

// // fixed colors
// LLPendulum2 {
// *draw {
// 	var bounds = view.bounds.width@view.bounds.height;
// 	var pt = 0@0;
// 	// TODO: export should rectify latents,
// 	// polarity of first latent is hardcoded here
// 	// var mag = 0 - latents[0];
// 	var mag = latents[0] * (l0_sign?1);
// 	// var mag = latents.squared.sum.sqrt/3-2;
// 	var ndrop = (latents.size - 1) % 4;
// 	mag = (mag.exp+1).log.sqrt()/2;
// 	Pen.color = Color(0,0,0,0.3);
// 	Pen.fillRect(Rect(0,0,view.bounds.width,view.bounds.height));
// 	latents.drop(1).drop(0-ndrop).clump(2).do{ |item,i|
// 		var new_pt;
// 		Pen.color = Color(
// 			(i*2pi/3).cos+1/2,
// 			(i*2pi/4).cos+1/2,
// 			(i*2pi/5).cos+1/2);
// 		Pen.moveTo(pt+1/2*bounds);
// 		new_pt = Point(item[0],item[1])/3/(i/3+1)*mag + pt;
// 		new_pt = new_pt / (new_pt.abs+1);
// 		Pen.lineTo(new_pt+1/2*bounds);
// 		pt = new_pt;
// 		Pen.width = bounds.x/10 * mag/(i/3+1);
// 		Pen.stroke;
// 	}
// }}
// // blocks
// LLBlocks {
// *draw {
// 	var bounds = view.bounds.width@view.bounds.height;
// 	var pt = 0@0;
// 	// TODO: export should rectify latents,
// 	// polarity of first latent is hardcoded here
// 	// var mag = 0 - latents[0];
// 	var mag = latents[0] * (l0_sign?1);
// 	// var mag = latents.squared.sum.sqrt/3-2;
// 	var ndrop = (latents.size - 1) % 4;
// 	mag = (mag.exp+1).log.sqrt()/2;
// 	Pen.color = Color(0,0,0,0.3);
// 	Pen.fillRect(Rect(0,0,view.bounds.width,view.bounds.height));
// 	latents.drop(1).drop(0-ndrop).clump(2).do{ |item,i|
// 		var start, end, len;
// 		Pen.color = Color(
// 			(i*2pi/3).cos+1/2,
// 			(i*2pi/4).cos+1/2,
// 			(i*2pi/5).cos+1/2);
// 		len = 1/(i+3);
// 		start = Point(item[0],item[1])/3*mag;
// 		end = start + (len@0);
// 		start = start - (len@0);
// 		Pen.moveTo(start+1/2*bounds);
// 		Pen.lineTo(end+1/2*bounds);
// 		Pen.width = bounds.x/10 * mag/(i/3+1);
// 		Pen.stroke;
// 	}
// }}

LLGUI {
	var <name;
	var <l0_sign;
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
	var <mapper;
	var <loops_bus;

	var <gui;

	ar { |input, blockSize=0|
		var out, zs; 
		var mx, tr;
		# out, zs = LivingLooper.ar(
			name, input, 
			loop:\loop.kr(0), thru:\thru.kr(0), auto:\auto.kr(0),
			blockSize:blockSize);
		// zs are packed in audio signals;

		mx = Mix.new(zs.abs);
		
		// use zs as its own trigger
		// nLatent.do{ |zi|
		// 	// var trig = DelayN.ar(mx, 0.1, SampleDur.ir*zi);
		// 	// SendReply.ar(trig, "/living_looper_monitor", zs, zi)
		// 	SendReply.ar(mx, "/living_looper_monitor_"++this.id, zs, zi);
		// 	mx = Delay1.ar(mx);
		// };

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
		theme = theme ? LLTheme.new;

		mapper = LLMIDIMapper.new;

		// loop buttons start / end recording
		loopButtons = nLoops.collect{ |i| 
			mapper.button(\loop++i.asSymbol).states_([
				["record "++(i+1).asString, theme.color_text, theme.color_fg],
				["play "++(i+1), theme.color_alert, theme.color_bg]])
				.font_(theme.font_button)
				.toolTip_("start/end recording loop"+(i+1));

		};
		// erase buttons 
		eraseButtons = nLoops.collect{ |i|
			mapper.button(\erase++i.asSymbol).states_([
				["erase "++i.asString,theme.color_text,theme.color_dark]])
				.font_(theme.font_button)
				.toolTip_("erase loop"+(i+1));
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
				debug.if{["LLGUI: loop", i+1].postln};
				// start recording
				synth!?(_.set(\loop, i+1));
				// any other recording stops internally -- 
				// make buttons reflect this
				loopButtons.do{ |b| b.value_(0)};
				b.value_(1);
			}{
				debug.if{["LLGUI: loop end"].postln};
				// end recording
				synth!?(_.set(\loop, 0));
			}
		}};

		eraseButtons.do{ |b,i| b.action_{ 
			// erase loop
			debug.if{["LLGUI: erase", i+1].postln};
			synth!?(_.set(\loop, -1-i));
			// any recordings stop
			loopButtons.do{ |b| b.value_(0)};
			displays[i].clearDrawing;
		}};

		autoButton.action_{
			(autoButton.value==1).if{
				debug.if{["LLGUI: auto mode", 2].postln};
				synth!?(_.set(\auto, 2))
			}{
				debug.if{["LLGUI: auto off"].postln};
				synth!?(_.set(\auto, 0))
			};
		};

		thruButton.action_{
			(thruButton.value==1).if{
				debug.if{["LLGUI: thru on"].postln};
				synth!?(_.set(\thru, 1))
			}{
				debug.if{["LLGUI: thru off"].postln};
				synth!?(_.set(\thru, 0))
			}
		};

		// GUI layout
		gui = View().layout_(
			VLayout(
				HLayout(
					thruButton,
					autoButton,
					mapper.gui
				).spacing_(theme.spacing),
				HLayout(
					*nLoops.collect{ |i| [VLayout(
						VLayout(
							[displays[i], stretch:5],
							[freqscopes[i], stretch:1],
						).spacing_(0),
						eraseButtons[i],
						loopButtons[i]
					).spacing_(theme.spacing), stretch:1]},
				).spacing_(theme.spacing),
			).margins_(0)
		)
	}

	destroy {
		gui.remove;
		synth !? (_.free);
		this.cleanup;
	}

	//// programmatic access to GUI actions
	erase { |idx| 
		(idx>0).if{
			this.eraseButtons[idx-1].action.defer;
		}{
			("ERROR: LLGUI: can't erase loop" + idx).postln;
		}
	}

	record { |idx| 
		(idx>0).if{
			{
				this.autoButton.valueAction_(0);
				this.loopButtons[idx-1].value_(0);
				this.loopButtons[idx-1].action.();
				this.loopButtons[idx-1].value_(1);
			}.defer;
		}{
			("ERROR: LLGUI: can't record loop" + idx).postln;
		}
	}

	end { |idx|
		(idx>0).if{
			{
				this.autoButton.valueAction_(0);
				this.loopButtons[idx-1].value_(1);
				this.loopButtons[idx-1].action.();
				this.loopButtons[idx-1].value_(0);
			}.defer;
		}{
			("ERROR: LLGUI: can't end loop" + idx).postln;
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
		// synth: synth containing a LivingLooper UGen
		this.synth = synth;
		this.make_window.layout_(this.gui);
		^ this
	}

	make_window {
		// GUI elements
		window = Window.new(bounds:Rect(200,250,1200,300))
			.background_(theme.color_bg)
			.onClose_{this.cleanup}
			.front;
		^ window;
	}

	cleanup {
		freqscopes ? freqscopes.do(_.kill);
	}
}

// LLGUI plus a server panel and default synth
// TODO: channel routing options
// TODO: global MIDI port, channel select?
LLStandalone {
	var <theme;
	var <window;
	var server_control;
	var model_picker;
	var input_picker;
	var output_picker;
	var meter_view;
	var force_dl;
	var input_gain_knob, dry_gain_knob, output_gain_knob;

	var title;
	var <ll;

	var midi_state;

	var hsize = 1200;

	*new { |...args|
		// NOTE: best for apple silicon
		"OMP_NUM_THREADS".setenv("1"); 

		^super.newCopyArgs(*args).init;
	}

	make_meter {
		meter_view.layout_(VLayout(LLMeter(
			server_control.server, 
			numIns:server_control.inchan_box.value.asInteger,
			numOuts:server_control.outchan_box.value.asInteger,
			).view))
		;
	}

	make_synth {
		server_control.server.serverRunning.if{
			var n_out = 2; // TODO control this

			input_picker.items_(
				server_control.server.options.numInputBusChannels
				.collect{ |i| i+1 });

			output_picker.items_(
				(server_control.server.options.numOutputBusChannels+1-n_out)
				.collect{ |i| "%-%".format(i+1, i+n_out) });

			// meter.notNil.if{meter.remove};
			// meter = make_meter;
			meter_view.removeAll;
			this.make_meter;


			ll = LLGUI(\standalone);
			// copy previous MIDI mapper state over
			midi_state.notNil.if{ 
				ll.mapper.map.putAll(midi_state); 
				ll.mapper.set_states;
			};
			// create Synth on the server
			ll.synth = SynthDef(\ll++ll.id, {
				var in = SoundIn.ar(\inbus.kr(input_picker.value))
					* \input_gain.kr(input_gain_knob.value);
				var out = ll.ar(in, blockSize:2048);
				var stereo = \dry_gain.kr(dry_gain_knob.value)/2.sqrt * in 
					+ Splay.ar(out);
				stereo = Limiter.ar(
					stereo * 2 * \output_gain.kr(output_gain_knob.value));
				Out.ar(\outbus.kr(output_picker.value), stereo);
				Out.ar(ll.loops_bus, out);
			}).play;

			// add GUI to window
			window.layout.add(ll.gui, stretch:1);
			// increase window size
			window.setInnerExtent(hsize, 560);

		}
	}

	stop_synth {
		ll.notNil.if{
			midi_state = ll.mapper.map;
			ll.destroy;
		};
	}

	install {
		LivingLooper.detectNN.not.if{
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
						LivingLooper.installNN;
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
			"restarting interpreter...".postln;
			thisProcess.recompile;
			cond.test=false; cond.wait;
		};
	}

	init {
		// put these startup steps in a Routine, 
		// so that when it is played on the AppClock,
		// GUI calls and Condition.wait are both available
		var force_dl_ = false;
		var r = Routine{
			this.install;
			"-----LOAD-----".postln;
			LivingLooper.load(
				\standalone, model_picker.item, 
				forceDownload: force_dl_);
			force_dl = false;
			"-----STOP-----".postln;
			this.stop_synth;
			"-----MAKE-----".postln;
			this.make_synth;
		};

		theme = theme ? LLTheme.new;

		server_control = LLServerControl.new(
			Server.default, {r.reset; r.play(AppClock)});

		model_picker = PopUpMenu()
		// .allowsReselection_(true)
		.minWidth_(270)
		.items_(LivingLooper.sources.keys.asList++["..."])
		.background_(theme.color_highlight)
		.stringColor_(theme.color_dark)
		.toolTip_("choose a Living Looper model")
		.action_{
			(model_picker.item=="...").if{
				Dialog.openPanel({ |path|
					model_picker.items = [path] ++ model_picker.items;
					model_picker.valueAction_(0);
				})
			}{
				r.reset; r.play(AppClock);
			}
		};

		force_dl = Button()
		.states_([["dl", theme.color_alert, theme.color_fg]])
		.maxWidth_(25)
		.toolTip_("force download of current model (to get updates)")
		.action_{
			force_dl_ = true;
			r.reset; r.play(AppClock);
		};

		input_picker = PopUpMenu()
		// .minWidth_(300)
		.items_([1,2]) // TODO: update this when the server boots
		.background_(theme.color_bg)
		.stringColor_(theme.color_text)
		.toolTip_("choose a mono input channel")
		.action_{ll.synth.set(\inbus, input_picker.value)}
		;
		output_picker = PopUpMenu()
		// .minWidth_(300)
		.items_(["1-2", "2-3", "3-4"]) // TODO
		.background_(theme.color_bg)
		.stringColor_(theme.color_text)
		.toolTip_("choose a range of output channels")
		.action_{ll.synth.set(\outbus, output_picker.value)}
		;

		meter_view = View().maxHeight_(80);
		this.make_meter;

		title = VLayout(
			StaticText()
			.string_("Living Looper")
			.stringColor_(theme.color_highlight)
			.font_(Font("Helvetica", 60)),
			StaticText()
			.string_("v1.0.0b")
			.stringColor_(theme.color_highlight)
			.font_(Font("Helvetica", 32)),
		).spacing_(0);

		input_gain_knob = Knob()
		.color_(theme.knob_colors)
		.mode_(\vert)
		.action_{
			ll.notNil.if{ll.synth.set(\input_gain, input_gain_knob.value)}
		}
		.toolTip_("input gain")
		.value_(0.5);

		dry_gain_knob = Knob()
		.color_(theme.knob_colors)
		.mode_(\vert)
		.action_{
			ll.notNil.if{ll.synth.set(\dry_gain, dry_gain_knob.value)}
		}
		.toolTip_("dry gain relative to input")
		.value_(0);

		output_gain_knob = Knob()
		.color_(theme.knob_colors)
		.mode_(\vert)
		.action_{
			ll.notNil.if{ll.synth.set(\output_gain, output_gain_knob.value)}
		}
		.toolTip_("master output gain")
		.value_(0.5);

		window = Window.new("Living Looper", bounds:Rect(200, 500, hsize, 150))
		.background_(theme.color_bg)
		.layout_(VLayout(
			[HLayout(
				[title, stretch:2, align:\center],
				[VLayout(
					theme.label(
						HLayout(model_picker, force_dl), 
					"Model", view:true),
					HLayout(
						theme.label(input_picker, "Input", view:true),
						meter_view,
						theme.label(output_picker, "Output", view:true)
					)
				), stretch:1, align:\center],
				[server_control.gui, stretch:0],
			), stretch:0],
			[HLayout(
				theme.label(input_gain_knob, "Input", view:true), 
				theme.label(dry_gain_knob, "Dry", view:true),
				theme.label(output_gain_knob, "Output", view:true),
			), stretch:0]
		))
		.onClose_{
			ll.notNil.if{ll.cleanup};
		}
		.front;

	}
}