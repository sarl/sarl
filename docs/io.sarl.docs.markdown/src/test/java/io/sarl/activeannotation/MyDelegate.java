package io.sarl.activeannotation;

import java.util.UUID;

import io.sarl.lang.annotation.SarlSpecification;
import io.sarl.lang.core.Agent;
import io.sarl.lang.core.BuiltinCapacitiesProvider;

@SuppressWarnings("all")
public class MyDelegate implements SomeInterface {
	public int function(String param) {
		return 0;
	}
}
