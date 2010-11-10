package com.smallcultfollowing.lathos;

import java.io.IOException;


public class NoneCustomOutput implements CustomOutput {
	
	public static final NoneCustomOutput instance = new NoneCustomOutput();

	@Override
	public void renderInLine(Output output) throws IOException {
	}

}
