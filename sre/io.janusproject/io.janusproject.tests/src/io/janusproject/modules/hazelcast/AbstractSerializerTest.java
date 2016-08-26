/*
 * $Id$
 * 
 * Janus platform is an open-source multiagent platform.
 * More details on http://www.janusproject.io
 * 
 * Copyright (C) 2014-2015 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
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
package io.janusproject.modules.hazelcast;

import static org.junit.Assert.assertEquals;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.nio.ByteOrder;

import com.hazelcast.nio.ObjectDataInput;
import com.hazelcast.nio.ObjectDataOutput;
import com.hazelcast.nio.serialization.Data;
import com.hazelcast.nio.serialization.StreamSerializer;

import io.janusproject.testutils.AbstractJanusTest;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
abstract class AbstractSerializerTest extends AbstractJanusTest {

	protected static <S> void assertWriteRead(S expected, StreamSerializer<S> serializer) throws Exception {
		byte[] content;
		try (OutputStream os = new OutputStream()) {
			serializer.write(os, expected);
			content = os.toByteArray();
		}

		S newObj;
		try (InputStream is = new InputStream(content)) {
			newObj = serializer.read(is);
		}

		assertEquals(expected, newObj);
	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private static class OutputStream extends ByteArrayOutputStream implements ObjectDataOutput {

		/**
		 */
		public OutputStream() {
			//
		}

		/**
		 * {@inheritDoc}
		 */
		@Override
		public void writeBoolean(boolean v) throws IOException {
			throw new UnsupportedOperationException();
		}

		/**
		 * {@inheritDoc}
		 */
		@Override
		public void writeByte(int v) throws IOException {
			throw new UnsupportedOperationException();
		}

		/**
		 * {@inheritDoc}
		 */
		@Override
		public void writeShort(int v) throws IOException {
			throw new UnsupportedOperationException();
		}

		/**
		 * {@inheritDoc}
		 */
		@Override
		public void writeChar(int v) throws IOException {
			throw new UnsupportedOperationException();
		}

		/**
		 * {@inheritDoc}
		 */
		@Override
		public void writeInt(int v) throws IOException {
			throw new UnsupportedOperationException();
		}

		/**
		 * {@inheritDoc}
		 */
		@Override
		public void writeLong(long v) throws IOException {
			throw new UnsupportedOperationException();
		}

		/**
		 * {@inheritDoc}
		 */
		@Override
		public void writeFloat(float v) throws IOException {
			throw new UnsupportedOperationException();
		}

		/**
		 * {@inheritDoc}
		 */
		@Override
		public void writeDouble(double v) throws IOException {
			throw new UnsupportedOperationException();
		}

		/**
		 * {@inheritDoc}
		 */
		@Override
		public void writeBytes(String s) throws IOException {
			throw new UnsupportedOperationException();
		}

		/**
		 * {@inheritDoc}
		 */
		@Override
		public void writeChars(String s) throws IOException {
			throw new UnsupportedOperationException();
		}

		/**
		 * {@inheritDoc}
		 */
		@Override
		public void writeUTF(String s) throws IOException {
			writeObject(s);
		}

		/**
		 * {@inheritDoc}
		 */
		@Override
		public void writeCharArray(char[] chars) throws IOException {
			throw new UnsupportedOperationException();
		}

		/**
		 * {@inheritDoc}
		 */
		@Override
		public void writeIntArray(int[] ints) throws IOException {
			throw new UnsupportedOperationException();
		}

		/**
		 * {@inheritDoc}
		 */
		@Override
		public void writeLongArray(long[] longs) throws IOException {
			throw new UnsupportedOperationException();
		}

		/**
		 * {@inheritDoc}
		 */
		@Override
		public void writeDoubleArray(double[] values) throws IOException {
			throw new UnsupportedOperationException();
		}

		/**
		 * {@inheritDoc}
		 */
		@Override
		public void writeFloatArray(float[] values) throws IOException {
			throw new UnsupportedOperationException();
		}

		/**
		 * {@inheritDoc}
		 */
		@Override
		public void writeShortArray(short[] values) throws IOException {
			throw new UnsupportedOperationException();
		}

		/**
		 * {@inheritDoc}
		 */
		@Override
		public void writeObject(Object object) throws IOException {
			new ObjectOutputStream(this).writeObject(object);
		}

		/**
		 * {@inheritDoc}
		 */
		@Override
		public ByteOrder getByteOrder() {
			throw new UnsupportedOperationException();
		}

		/**
		 * {@inheritDoc}
		 */
		@Override
		public void writeByteArray(byte[] arg0) throws IOException {
			throw new UnsupportedOperationException();
		}

		/**
		 * {@inheritDoc}
		 */
		@Override
		public void writeData(Data arg0) throws IOException {
			throw new UnsupportedOperationException();
		}

		/**
		 * {@inheritDoc}
		 */
		@Override
		public void writeBooleanArray(boolean[] booleans) throws IOException {
			throw new UnsupportedOperationException();
		}

		/**
		 * {@inheritDoc}
		 */
		@Override
		public void writeUTFArray(String[] values) throws IOException {
			throw new UnsupportedOperationException();
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private static class InputStream extends ByteArrayInputStream implements ObjectDataInput {

		public InputStream(byte[] t) {
			super(t);
		}

		/**
		 * {@inheritDoc}
		 */
		@Override
		public void readFully(byte[] b) throws IOException {
			throw new UnsupportedOperationException();
		}

		/**
		 * {@inheritDoc}
		 */
		@Override
		public void readFully(byte[] b, int off, int len) throws IOException {
			throw new UnsupportedOperationException();
		}

		/**
		 * {@inheritDoc}
		 */
		@Override
		public int skipBytes(int n) throws IOException {
			throw new UnsupportedOperationException();
		}

		/**
		 * {@inheritDoc}
		 */
		@Override
		public boolean readBoolean() throws IOException {
			throw new UnsupportedOperationException();
		}

		/**
		 * {@inheritDoc}
		 */
		@Override
		public byte readByte() throws IOException {
			throw new UnsupportedOperationException();
		}

		/**
		 * {@inheritDoc}
		 */
		@Override
		public int readUnsignedByte() throws IOException {
			throw new UnsupportedOperationException();
		}

		/**
		 * {@inheritDoc}
		 */
		@Override
		public short readShort() throws IOException {
			throw new UnsupportedOperationException();
		}

		/**
		 * {@inheritDoc}
		 */
		@Override
		public int readUnsignedShort() throws IOException {
			throw new UnsupportedOperationException();
		}

		/**
		 * {@inheritDoc}
		 */
		@Override
		public char readChar() throws IOException {
			throw new UnsupportedOperationException();
		}

		/**
		 * {@inheritDoc}
		 */
		@Override
		public int readInt() throws IOException {
			throw new UnsupportedOperationException();
		}

		/**
		 * {@inheritDoc}
		 */
		@Override
		public long readLong() throws IOException {
			throw new UnsupportedOperationException();
		}

		/**
		 * {@inheritDoc}
		 */
		@Override
		public float readFloat() throws IOException {
			throw new UnsupportedOperationException();
		}

		/**
		 * {@inheritDoc}
		 */
		@Override
		public double readDouble() throws IOException {
			throw new UnsupportedOperationException();
		}

		/**
		 * {@inheritDoc}
		 */
		@Override
		public String readLine() throws IOException {
			throw new UnsupportedOperationException();
		}

		/**
		 * {@inheritDoc}
		 */
		@Override
		public String readUTF() throws IOException {
			return (String) readObject();
		}

		/**
		 * {@inheritDoc}
		 */
		@Override
		public char[] readCharArray() throws IOException {
			throw new UnsupportedOperationException();
		}

		/**
		 * {@inheritDoc}
		 */
		@Override
		public int[] readIntArray() throws IOException {
			throw new UnsupportedOperationException();
		}

		/**
		 * {@inheritDoc}
		 */
		@Override
		public long[] readLongArray() throws IOException {
			throw new UnsupportedOperationException();
		}

		/**
		 * {@inheritDoc}
		 */
		@Override
		public double[] readDoubleArray() throws IOException {
			throw new UnsupportedOperationException();
		}

		/**
		 * {@inheritDoc}
		 */
		@Override
		public float[] readFloatArray() throws IOException {
			throw new UnsupportedOperationException();
		}

		/**
		 * {@inheritDoc}
		 */
		@Override
		public short[] readShortArray() throws IOException {
			throw new UnsupportedOperationException();
		}

		/**
		 * {@inheritDoc}
		 */
		@Override
		public <T> T readObject() throws IOException {
			try {
				return (T) new ObjectInputStream(this).readObject();
			} catch (ClassNotFoundException e) {
				throw new IOException(e);
			}
		}

		/**
		 * {@inheritDoc}
		 */
		@Override
		public ClassLoader getClassLoader() {
			return InputStream.class.getClassLoader();
		}

		/**
		 * {@inheritDoc}
		 */
		@Override
		public ByteOrder getByteOrder() {
			throw new UnsupportedOperationException();
		}

		/**
		 * {@inheritDoc}
		 */
		@Override
		public byte[] readByteArray() throws IOException {
			throw new UnsupportedOperationException();
		}

		/**
		 * {@inheritDoc}
		 */
		@Override
		public Data readData() throws IOException {
			throw new UnsupportedOperationException();
		}

		/**
		 * {@inheritDoc}
		 */
		@Override
		public boolean[] readBooleanArray() throws IOException {
			throw new UnsupportedOperationException();
		}

		/**
		 * {@inheritDoc}
		 */
		@Override
		public String[] readUTFArray() throws IOException {
			throw new UnsupportedOperationException();
		}

	}

}
