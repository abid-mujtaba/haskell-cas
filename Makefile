#  Copyright 2015 Abid Hasan Mujtaba
#
#  Licensed under the Apache License, Version 2.0 (the "License");
#  you may not use this file except in compliance with the License.
#  You may obtain a copy of the License at
#
#      http://www.apache.org/licenses/LICENSE-2.0
#
#  Unless required by applicable law or agreed to in writing, software
#  distributed under the License is distributed on an "AS IS" BASIS,
#  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#  See the License for the specific language governing permissions and
#  limitations under the License.
#
#  
#  This is the Makefile which provides targets for common actions that one can perform with this project.

# We provide a list of phony targets which specify actions that are not based on changes in the code-base
.PHONY: clean

clean:				# Clean the compilation by-products (.hi and .o files and executables)
	rm *.hi	*.o TestCAS
