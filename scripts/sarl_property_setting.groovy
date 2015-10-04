now = session.getStartTime();

/* Bug in Jenkins/Hudson: the
 * buildId property is not correctly set
 * since
 * maven.build.timestamp is skipped by Jenkins/Hudson.
 */
if (project.properties.buildId == "${maven.build.timestamp}") {
	project.properties.buildId = (new java.text.SimpleDateFormat("yyyyMMddHHmmss")).format(now)
	println("\tbuildId: " + project.properties.buildId + " (reset for Hudson)")
} else {
	println("\tbuildId: " + project.properties.buildId)
}

snapshotTag = ""

if (project.version.endsWith(snapshotTag)) {
	len = project.version.size() - snapshotTag.size()
	version = project.version.substring(0, len)
} else {
	version = project.version
}

versionFields = version.split("\\.")

project.properties.sarlspecificationreleaseversion = versionFields[0] + "." + versionFields[1]
println("\tsarlspecificationreleaseversion: " + project.properties.sarlspecificationreleaseversion)

project.properties.sarlreleaseversion = project.properties.sarlspecificationreleaseversion + "." + versionFields[2]
println("\tsarlreleaseversion: " + project.properties.sarlreleaseversion)

project.properties.sarlspecificationreleasedate = (new java.text.SimpleDateFormat("yyyy-MM-dd")).format(now)
println("\tsarlspecificationreleasedate: " + project.properties.sarlspecificationreleasedate)
