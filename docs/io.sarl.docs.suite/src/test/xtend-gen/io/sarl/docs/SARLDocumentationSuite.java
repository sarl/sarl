package io.sarl.docs;

import io.sarl.docs.GettingStartedSuite;
import io.sarl.docs.ReferenceDocumentationSuite;
import org.jnario.runner.Contains;
import org.jnario.runner.ExampleGroupRunner;
import org.jnario.runner.Named;
import org.junit.runner.RunWith;

@Named("SARL Documentation")
@Contains({ GettingStartedSuite.class, ReferenceDocumentationSuite.class })
@RunWith(ExampleGroupRunner.class)
@SuppressWarnings("all")
public class SARLDocumentationSuite {
}
