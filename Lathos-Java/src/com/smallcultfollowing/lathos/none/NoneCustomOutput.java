package com.smallcultfollowing.lathos.none;

import java.io.IOException;

import com.smallcultfollowing.lathos.model.CustomOutput;
import com.smallcultfollowing.lathos.model.Output;

public class NoneCustomOutput implements CustomOutput {
	
	public static final NoneCustomOutput instance = new NoneCustomOutput();

	@Override
	public void renderInLine(Output output) throws IOException {
	}

}
