package com.smallcultfollowing.lathos.context;

import java.util.LinkedList;

import com.smallcultfollowing.lathos.model.Line;
import com.smallcultfollowing.lathos.model.Page;
import com.smallcultfollowing.lathos.model.LathosServer;
import com.smallcultfollowing.lathos.server.LineImpl;
import com.smallcultfollowing.lathos.server.UserPage;

public class AbstractContext implements Context {
	
	private final LathosServer server;
	private final LinkedList<Page> stack = new LinkedList<Page>();
	private Line previousLine;
	
	public AbstractContext(LathosServer server) {
		this.server = server;
	}
	
	private String supplyDefault(String id) {
		if(id != null)
			return id;
		return freshId();
	}

	@Override
	public Page pushTopLevel(String id, Object... title) {
		id = supplyDefault(id);
		
		Page page = new UserPage(id, null);
		stack.add(page);
		server.addTopLevelPage(page);
				
		addTitle(title);
		
		return page;
	}
	
	@Override
	public Page pushChild(String id, Object... title) {
		if(stack.isEmpty())
			return pushTopLevel(id, title);
		
		id = supplyDefault(id);
		Page page = new UserPage(id, stack.getLast());
		stack.add(page);
		
		addTitle(title);
		
		return page;
	}

	private void addTitle(Object[] title) {
		previousLine = new LineImpl(true);
		stack.getLast().addContent(previousLine);
		append(title);
	}

	@Override
	public void push(Page page) {
		stack.add(page);
	}

	@Override
	public void embedIn(Page page) {
		page.addContent(stack.getLast());
	}

	@Override
	public void log(Object... line) {
		previousLine = new LineImpl();
		stack.getLast().addContent(previousLine);
		append(line);
	}

	@Override
	public void append(Object... line) {
		for(Object o : line) {
			server.addToLine(previousLine, o);
		}
	}

	@Override
	public void pop(Page page) {
		if(page != null) {
			if(stack.getLast() != page)
				throw new RuntimeException(
						String.format(
								"Popping %s but passed in %s", 
								stack.getLast(),
								page));
		}
		
		stack.removeLast();
	}

	@Override
	public String freshId() {
		return server.freshId();
	}

	@Override
	public LathosServer server() {
		return server;
	}

	@Override
	public Context context() {
		return server.context();
	}

}
