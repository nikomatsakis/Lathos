package com.smallcultfollowing.lathos.model;

import java.util.LinkedList;
import java.util.List;


public class DefaultContext implements Context {
	
	private final LathosServer server;
	private final LinkedList<Page> stack = new LinkedList<Page>();
	private Line previousLine;
	
	public DefaultContext(LathosServer server, List<Page> initialStack) {
		this.server = server;
		stack.addAll(initialStack);
	}
	
	private String supplyDefault(String id) {
		if(id != null)
			return id;
		return freshId();
	}
	
	private void addToLine(Line line, Object... objects) {
		for(Object o : objects) 
			server.addToLine(line, o);
	}

	@Override
	public Page pushTopLevel(String id, Object... title) {
		id = supplyDefault(id);
		
		Page page = new UserPage(id, null);
		stack.add(page);
		server.registerPage(page);
				
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

	private void addTitle(Object[] contents) {
		Title title = new Title();
		previousLine = title;
		stack.getLast().addContent(title);
		append(contents);
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
	public void log(Object... contents) {
		Para para = new Para();
		previousLine = para;
		stack.getLast().addContent(para);
		append(contents);
	}

	@Override
	public void append(Object... contents) {
		addToLine(previousLine, contents);
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
		return new DefaultContext(server, stack);
	}

	@Override
	public CustomOutput link(Page page, Object... content) {
		Link result = new Link(page);
		addToLine(result, content);
		return result;
	}

	@Override
	public Page topPage() {
		return stack.getLast();
	}

}
