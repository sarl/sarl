/*
 * $Id$
 *
 * Janus platform is an open-source multiagent platform.
 * More details on http://www.janusproject.io
 *
 * Copyright (C) 2014-2015 the original authors or authors.
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

package io.janusproject.services.network;

import java.io.IOError;
import java.net.Inet4Address;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.NetworkInterface;
import java.net.SocketException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.UnknownHostException;
import java.util.Enumeration;
import java.util.UUID;

/**
 * Provide utilities related to the network.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public final class NetworkUtil {

	private NetworkUtil() {
		//
	}

	/**
	 * Replies if the host is connected.
	 *
	 * @return <code>true</code> if this host has one non-loopback address; <code>false</code> otherwise.
	 */
	public static boolean isConnectedHost() {
		return getPrimaryAddress() != null;
	}

	/**
	 * Replies the first IPv4 public address. A public address is an address that is not loopback.
	 *
	 * @return the first public IPv4 address or <code>null</code> if none.
	 */
	public static InetAddress getPrimaryAddress() {
		try {
			Enumeration<NetworkInterface> interfaces = NetworkInterface.getNetworkInterfaces();
			if (interfaces != null) {
				NetworkInterface inter;
				InetAddress adr;
				Enumeration<InetAddress> addrs;
				while (interfaces.hasMoreElements()) {
					inter = interfaces.nextElement();
					addrs = inter.getInetAddresses();
					if (addrs != null) {
						while (addrs.hasMoreElements()) {
							adr = addrs.nextElement();
							if (adr != null && !adr.isLoopbackAddress() && (adr instanceof Inet4Address)) {
								return adr;
							}
						}
					}
				}
			}
		} catch (SocketException e) {
			//
		}
		return null;
	}

	/**
	 * Replies the IPv4 loopback address.
	 *
	 * @return the IPv4 loopback address or <code>null</code> if none.
	 */
	public static InetAddress getLoopbackAddress() {
		try {
			Enumeration<NetworkInterface> interfaces = NetworkInterface.getNetworkInterfaces();
			if (interfaces != null) {
				NetworkInterface inter;
				InetAddress adr;
				Enumeration<InetAddress> addrs;
				while (interfaces.hasMoreElements()) {
					inter = interfaces.nextElement();
					addrs = inter.getInetAddresses();
					if (addrs != null) {
						while (addrs.hasMoreElements()) {
							adr = addrs.nextElement();
							if (adr != null && adr.isLoopbackAddress() && (adr instanceof Inet4Address)) {
								return adr;
							}
						}
					}
				}
			}
		} catch (SocketException e) {
			//
		}
		return null;
	}

	/**
	 * Replies the byte-array representation of the given id.
	 *
	 * @param id - the UUID to convert to byte array.
	 * @return the byte-array representation.
	 */
	public static byte[] toByteArray(UUID id) {
		return id.toString().getBytes(NetworkConfig.getStringEncodingCharset());
	}

	/**
	 * Replies the id from the given byte-array representation.
	 *
	 * @param id - the byte array to convert to UUID.
	 * @return the UUID.
	 */
	public static UUID fromByteArray(byte[] id) {
		return UUID.fromString(new String(id, NetworkConfig.getStringEncodingCharset()));
	}

	/**
	 * Convert a string URI to an object URI.
	 *
	 * <p>
	 * This function support the syntax ":*" for the port.
	 *
	 * @param uri - the string representation of the URI to parse.
	 * @return the URI.
	 * @throws URISyntaxException - if the given string has invalid format.
	 */
	public static URI toURI(String uri) throws URISyntaxException {
		URI u = new URI(uri);
		// Inspired by ZeroMQ
		String adr = u.getAuthority();
		if (adr == null) {
			adr = u.getPath();
		}
		if (adr != null && adr.endsWith(":*")) { //$NON-NLS-1$
			return new URI(u.getScheme(), u.getUserInfo(), adr.substring(0, adr.length() - 2), -1, null, u.getQuery(),
					u.getFragment());
		}
		return u;
	}

	/**
	 * Convert an inet address to an URI.
	 *
	 * @param adr - address to convert to URI.
	 * @return the URI.
	 */
	public static URI toURI(InetAddress adr) {
		try {
			return new URI("tcp", adr.getHostAddress(), null, null); //$NON-NLS-1$
		} catch (URISyntaxException e) {
			throw new IOError(e);
		}
	}

	/**
	 * Convert a socket address to an URI.
	 *
	 * @param adr - address to convert to URI.
	 * @return the URI.
	 */
	public static URI toURI(InetSocketAddress adr) {
		return toURI(adr.getAddress(), adr.getPort());
	}

	/**
	 * Convert an inet address to an URI.
	 *
	 * @param adr - the address.
	 * @param port - port number, if negative or nul, use the "*" notation.
	 * @return the URI.
	 */
	public static URI toURI(InetAddress adr, int port) {
		try {
			return new URI("tcp", null, adr.getHostAddress(), port, null, null, null); //$NON-NLS-1$
		} catch (URISyntaxException e) {
			throw new IOError(e);
		}
	}

	/**
	 * Extract an Inet address from an URI.
	 *
	 * @param uri - the address.
	 * @return the address.
	 */
	public static InetAddress toInetAddress(URI uri) {
		try {
			// Copy/paste from the ZeroMQ lib
			String protocol = uri.getScheme();
			String address = uri.getHost();
			if ("tcp".equalsIgnoreCase(protocol)) { //$NON-NLS-1$
				return InetAddress.getByName(address);
			}
			throw new IllegalArgumentException(uri.toString());
		} catch (UnknownHostException e) {
			throw new IOError(e);
		}
	}

	/**
	 * Extract an Inet address from an URI.
	 *
	 * @param uri - the address.
	 * @return the address.
	 */
	public static InetSocketAddress toInetSocketAddress(URI uri) {
		InetAddress adr = toInetAddress(uri);
		int port = uri.getPort();
		if (port <= 0) {
			throw new IllegalArgumentException(uri.toString());
		}
		return new InetSocketAddress(adr, port);
	}

}
