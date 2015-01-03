/*
 * $Id$
 * 
 * Janus platform is an open-source multiagent platform.
 * More details on http://www.janusproject.io
 * 
 * Copyright (C) 2014 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package io.sarl.util.tests;

import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import io.sarl.tests.api.AbstractSarlTest;
import io.sarl.tests.api.Nullable;
import io.sarl.util.Collections3;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings({"javadoc","synthetic-access"})
public class Collections3_SynchronizedCollectionTest_Sync extends AbstractSarlTest {

	@Nullable
	private ExecutorService executors;
	@Nullable
	private Object mutex;
	@Nullable
	private List<String> original;
	@Nullable
	private Collection<String> collection;
	
	@Before
	public void setUp() {
		this.executors = Executors.newFixedThreadPool(5);
		this.mutex = new Object();
		this.original = new ArrayList<>();
		for(int i=0; i<50; ++i) {
			this.original.add("0x"+Double.toHexString(Math.random())); //$NON-NLS-1$
		}
		this.collection = Collections3.synchronizedCollection(this.original, this.mutex);
	}
	
	@After
	public void tearDown() throws Exception {
		this.executors.shutdownNow();
		this.executors.awaitTermination(30, TimeUnit.SECONDS);
	}

	@Test
    public void iteratorHasMutex() throws Exception {
    	synchronized(this.mutex) {
    		this.executors.submit(new Runnable() {
				@Override
				public void run() {
					for(int i=0; i<10; ++i) {
						Collections3_SynchronizedCollectionTest_Sync.this.collection.add(Integer.toString(i));
					}
				}
			});
    		Iterator<String> it = this.collection.iterator();
    		int i = 0;
    		while(it.hasNext()) {
    			String s = it.next();
    			assertSame(this.original.get(i), s);
    			++i;
    		}
    	}
    	this.executors.shutdown();
    	this.executors.awaitTermination(30, TimeUnit.SECONDS);
    	for(int i=0; i<10; ++i) {
    		assertTrue(this.collection.contains(Integer.toString(i)));
    	}
    }
    
}
