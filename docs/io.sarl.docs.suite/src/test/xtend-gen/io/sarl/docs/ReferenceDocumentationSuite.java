package io.sarl.docs;

import io.sarl.docs.reference.EventReferenceSpec;
import io.sarl.docs.reference.GeneralSyntaxReferenceSpec;
import org.jnario.runner.Contains;
import org.jnario.runner.ExampleGroupRunner;
import org.jnario.runner.Named;
import org.junit.runner.RunWith;

@Named("Reference Documentation")
@Contains({ EventReferenceSpec.class, GeneralSyntaxReferenceSpec.class })
@RunWith(ExampleGroupRunner.class)
@SuppressWarnings("all")
public class ReferenceDocumentationSuite {
}
